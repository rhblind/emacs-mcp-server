;;; test-mcp-security.el --- Security Tests for MCP Server -*- lexical-binding: t; -*-

;;; Commentary:
;; Comprehensive unit tests for mcp-server-security module.
;;
;; These tests cover:
;;   - mcp-server-security--is-sensitive-file: path expansion and pattern matching
;;   - mcp-server-security--is-dangerous-operation: symbol and string inputs
;;   - mcp-server-security--check-form-safety: form-level access control
;;   - Issue #9 regression: find-file on sensitive files must be blocked
;;     even when find-file is in mcp-server-security-allowed-dangerous-functions
;;   - Permission caching and audit logging
;;   - Input validation

;;; Code:

(require 'ert)
(require 'test-helpers)
(require 'mcp-server-security)

;; Load eval-elisp tool for integration tests
(let* ((this-file (or load-file-name buffer-file-name))
       (tools-dir (and this-file
                       (expand-file-name "../../tools"
                                         (file-name-directory this-file)))))
  (when tools-dir
    (add-to-list 'load-path tools-dir)))
(require 'mcp-server-tools)
(require 'mcp-server-emacs-tools-eval-elisp)

;;; Tests for mcp-server-security--is-sensitive-file

(ert-deftest mcp-security-test-sensitive-file-nil-for-safe-paths ()
  "Non-sensitive paths must not be flagged as sensitive."
  (let ((mcp-server-security-sensitive-file-patterns '("~/.ssh/" "~/.gnupg/"))
        (mcp-server-security-allowed-sensitive-files nil))
    (should-not (mcp-server-security--is-sensitive-file "/tmp/safe-file.txt"))
    (should-not (mcp-server-security--is-sensitive-file "~/Documents/notes.org"))
    (should-not (mcp-server-security--is-sensitive-file "/home/user/code/project.el"))))

(ert-deftest mcp-security-test-sensitive-file-tilde-prefix-patterns ()
  "Files under ~/prefix patterns must be detected as sensitive.
This is Bug 1: expand-file-name is called on the path but not on patterns,
so ~/. ssh/ patterns fail to match expanded /home/user/.ssh/id_rsa paths."
  (let ((mcp-server-security-sensitive-file-patterns '("~/.ssh/" "~/.gnupg/"))
        (mcp-server-security-allowed-sensitive-files nil))
    (should (mcp-server-security--is-sensitive-file "~/.ssh/id_rsa"))
    (should (mcp-server-security--is-sensitive-file "~/.ssh/config"))
    (should (mcp-server-security--is-sensitive-file "~/.gnupg/private-keys-v1.d"))))

(ert-deftest mcp-security-test-sensitive-file-expanded-paths ()
  "Already-expanded absolute paths must still match ~/prefix patterns."
  (let ((mcp-server-security-sensitive-file-patterns '("~/.ssh/" "~/.gnupg/"))
        (mcp-server-security-allowed-sensitive-files nil))
    (should (mcp-server-security--is-sensitive-file (expand-file-name "~/.ssh/id_rsa")))
    (should (mcp-server-security--is-sensitive-file (expand-file-name "~/.gnupg/pubring.kbx")))))

(ert-deftest mcp-security-test-sensitive-file-absolute-path-patterns ()
  "Absolute path patterns like /etc/passwd must match exactly."
  (let ((mcp-server-security-sensitive-file-patterns '("/etc/passwd" "/etc/shadow"))
        (mcp-server-security-allowed-sensitive-files nil))
    (should (mcp-server-security--is-sensitive-file "/etc/passwd"))
    (should (mcp-server-security--is-sensitive-file "/etc/shadow"))
    (should-not (mcp-server-security--is-sensitive-file "/etc/hosts"))))

(ert-deftest mcp-security-test-sensitive-file-filename-patterns ()
  "Bare filename patterns must match by filename regardless of directory."
  (let ((mcp-server-security-sensitive-file-patterns '("passwords" "secrets" "credentials"))
        (mcp-server-security-allowed-sensitive-files nil))
    (should (mcp-server-security--is-sensitive-file "/some/path/passwords"))
    (should (mcp-server-security--is-sensitive-file "/project/secrets"))
    (should (mcp-server-security--is-sensitive-file "/var/lib/credentials"))
    (should-not (mcp-server-security--is-sensitive-file "/project/notes.txt"))))

(ert-deftest mcp-security-test-sensitive-file-allowed-list-bypass ()
  "Files in mcp-server-security-allowed-sensitive-files must bypass the check."
  (let ((mcp-server-security-sensitive-file-patterns '("~/.ssh/"))
        (mcp-server-security-allowed-sensitive-files
         (list (expand-file-name "~/.ssh/known_hosts"))))
    ;; Explicitly allowed file: not sensitive
    (should-not (mcp-server-security--is-sensitive-file "~/.ssh/known_hosts"))
    ;; Other SSH files still blocked
    (should (mcp-server-security--is-sensitive-file "~/.ssh/id_rsa"))
    (should (mcp-server-security--is-sensitive-file "~/.ssh/id_ed25519"))))

(ert-deftest mcp-security-test-sensitive-file-nil-and-non-string-inputs ()
  "Nil and non-string inputs must return nil without signaling errors."
  (should-not (mcp-server-security--is-sensitive-file nil))
  (should-not (mcp-server-security--is-sensitive-file 42))
  (should-not (mcp-server-security--is-sensitive-file '(a b c))))

(ert-deftest mcp-security-test-sensitive-file-issue-9-exact-patterns ()
  "Reproduces the exact pattern configuration from issue #9.
User set: (setq mcp-server-security-sensitive-file-patterns
              '(\"~/.authinfo*\" \"~/.netrc*\" \"~/.ssh/\" \"~/.gnupg/\"))
and expects ~/.ssh/id_rsa to be blocked."
  (let ((mcp-server-security-sensitive-file-patterns
         '("~/.authinfo*" "~/.netrc*" "~/.ssh/" "~/.gnupg/"))
        (mcp-server-security-allowed-sensitive-files nil))
    (should (mcp-server-security--is-sensitive-file "~/.ssh/id_rsa"))
    (should (mcp-server-security--is-sensitive-file "~/.gnupg/secring.gpg"))))

;;; Tests for mcp-server-security--is-dangerous-operation

(ert-deftest mcp-security-test-dangerous-op-blocked-symbols ()
  "Functions listed in mcp-server-security-dangerous-functions are dangerous."
  (let ((mcp-server-security-dangerous-functions '(shell-command delete-file))
        (mcp-server-security-allowed-dangerous-functions nil))
    (should (mcp-server-security--is-dangerous-operation 'shell-command))
    (should (mcp-server-security--is-dangerous-operation 'delete-file))))

(ert-deftest mcp-security-test-dangerous-op-allowed-bypass ()
  "Functions in the allowed list must not be considered dangerous."
  (let ((mcp-server-security-dangerous-functions '(find-file shell-command))
        (mcp-server-security-allowed-dangerous-functions '(find-file)))
    (should-not (mcp-server-security--is-dangerous-operation 'find-file))
    (should (mcp-server-security--is-dangerous-operation 'shell-command))))

(ert-deftest mcp-security-test-dangerous-op-name-patterns ()
  "Functions matching delete/kill/remove/destroy patterns are dangerous."
  (let ((mcp-server-security-dangerous-functions nil)
        (mcp-server-security-allowed-dangerous-functions nil))
    (should (mcp-server-security--is-dangerous-operation 'delete-region))
    (should (mcp-server-security--is-dangerous-operation 'kill-buffer))
    (should (mcp-server-security--is-dangerous-operation 'remove-hook))
    (should (mcp-server-security--is-dangerous-operation 'destroy-window))))

(ert-deftest mcp-security-test-dangerous-op-safe-symbols ()
  "Symbols not in the dangerous list and not matching patterns are safe."
  (let ((mcp-server-security-dangerous-functions '(shell-command))
        (mcp-server-security-allowed-dangerous-functions nil))
    (should-not (mcp-server-security--is-dangerous-operation 'message))
    (should-not (mcp-server-security--is-dangerous-operation 'format))
    (should-not (mcp-server-security--is-dangerous-operation '+))))

(ert-deftest mcp-security-test-dangerous-op-string-sensitive-file-ops ()
  "String operation IDs from the sensitive file check path must not crash.
This is Bug 2: (symbol-name operation) throws wrong-type-argument when
operation is a string like \"access-sensitive-file:find-file\".
After the fix, these string ops must be recognised as dangerous (denied)."
  ;; String ops prefixed with access-sensitive- are always dangerous
  (should (mcp-server-security--is-dangerous-operation
           "access-sensitive-file:find-file"))
  (should (mcp-server-security--is-dangerous-operation
           "access-sensitive-buffer:with-current-buffer"))
  ;; An unrelated string is not dangerous
  (should-not (mcp-server-security--is-dangerous-operation "some-unrelated-op")))

;;; Tests for mcp-server-security--check-form-safety

(ert-deftest mcp-security-test-form-safety-blocks-dangerous-function ()
  "A form that calls a dangerous function must be blocked."
  (mcp-test-with-mock-server
   (let ((mcp-server-security-dangerous-functions '(delete-file))
         (mcp-server-security-allowed-dangerous-functions nil)
         (mcp-server-security-prompt-for-permissions nil))
     (should-error (mcp-server-security--check-form-safety
                    '(delete-file "/tmp/test.txt"))
                   :type 'error))))

(ert-deftest mcp-security-test-form-safety-allows-safe-functions ()
  "Forms using safe functions must pass without error."
  (mcp-test-with-mock-server
   (let ((mcp-server-security-dangerous-functions nil)
         (mcp-server-security-allowed-dangerous-functions nil))
     (mcp-server-security--check-form-safety '(+ 1 2))
     (mcp-server-security--check-form-safety '(message "hello"))
     (mcp-server-security--check-form-safety '(string-to-number "42")))))

(ert-deftest mcp-security-test-form-safety-allows-allowed-function-safe-file ()
  "An allowed function accessing a non-sensitive file must be permitted."
  (mcp-test-with-mock-server
   (let ((mcp-server-security-dangerous-functions '(find-file))
         (mcp-server-security-allowed-dangerous-functions '(find-file))
         (mcp-server-security-sensitive-file-patterns '("~/.ssh/"))
         (mcp-server-security-prompt-for-permissions nil))
     ;; /tmp/safe.txt does not match ~/.ssh/ pattern: must not error
     (mcp-server-security--check-form-safety '(find-file "/tmp/safe.txt")))))

(ert-deftest mcp-security-test-form-safety-blocks-allowed-function-sensitive-file ()
  "Sensitive file access must be blocked even when the function is in the allowed list.
This is the core regression test for issue #9."
  (mcp-test-with-mock-server
   (let ((mcp-server-security-dangerous-functions '(find-file))
         (mcp-server-security-allowed-dangerous-functions '(find-file))
         (mcp-server-security-sensitive-file-patterns '("~/.ssh/"))
         (mcp-server-security-prompt-for-permissions nil))
     (should-error
      (mcp-server-security--check-form-safety '(find-file "~/.ssh/id_rsa"))
      :type 'error))))

(ert-deftest mcp-security-test-form-safety-recursive-check ()
  "Security checks must recurse into nested sub-forms."
  (mcp-test-with-mock-server
   (let ((mcp-server-security-dangerous-functions '(delete-file))
         (mcp-server-security-allowed-dangerous-functions nil)
         (mcp-server-security-prompt-for-permissions nil))
     (should-error
      (mcp-server-security--check-form-safety
       '(progn (message "ok") (delete-file "/tmp/test.txt")))
      :type 'error))))

(ert-deftest mcp-security-test-form-safety-find-file-noselect-sensitive ()
  "find-file-noselect accessing a sensitive file must also be blocked."
  (mcp-test-with-mock-server
   (let ((mcp-server-security-dangerous-functions '(find-file-noselect))
         (mcp-server-security-allowed-dangerous-functions '(find-file-noselect))
         (mcp-server-security-sensitive-file-patterns '("~/.ssh/"))
         (mcp-server-security-prompt-for-permissions nil))
     (should-error
      (mcp-server-security--check-form-safety
       '(find-file-noselect "~/.ssh/id_rsa"))
      :type 'error))))

;;; Issue #9 end-to-end integration test

(ert-deftest mcp-security-test-issue-9-eval-elisp-blocks-ssh-key-access ()
  "Issue #9 regression: eval-elisp must return an error when asked to open
~/.ssh/id_rsa even when find-file is in mcp-server-security-allowed-dangerous-functions.
The Emacs-side sensitive file check must override the function-level allowance."
  (mcp-test-with-mock-server
   (let ((mcp-server-security-allowed-dangerous-functions '(find-file))
         (mcp-server-security-sensitive-file-patterns
          '("~/.authinfo*" "~/.netrc*" "~/.ssh/" "~/.gnupg/"))
         (mcp-server-security-prompt-for-permissions nil))
     (let ((result (mcp-server-emacs-tools--eval-elisp-handler
                    '((expression . "(find-file \"~/.ssh/id_rsa\")")))))
       (should (stringp result))
       (should (string-prefix-p "Error:" result))
       (should (string-match-p
                "[Pp]ermission\\|[Ss]ensitive\\|[Dd]enied" result))))))

(ert-deftest mcp-security-test-issue-9-safe-eval-blocks-directly ()
  "mcp-server-security-safe-eval must raise an error for sensitive file access."
  (mcp-test-with-mock-server
   (let ((mcp-server-security-allowed-dangerous-functions '(find-file))
         (mcp-server-security-sensitive-file-patterns '("~/.ssh/"))
         (mcp-server-security-prompt-for-permissions nil))
     (should-error
      (mcp-server-security-safe-eval '(find-file "~/.ssh/id_rsa"))
      :type 'error))))

;;; Tests for permission caching

(ert-deftest mcp-security-test-permission-cache-records-decision ()
  "Permission decisions must be stored in the cache."
  (mcp-test-with-mock-server
   (let ((mcp-server-security-dangerous-functions nil)
         (mcp-server-security-allowed-dangerous-functions nil))
     (mcp-server-security-check-permission 'safe-op "some-data")
     (should (hash-table-p mcp-server-security--permission-cache))
     (should (gethash "safe-op:some-data"
                      mcp-server-security--permission-cache)))))

(ert-deftest mcp-security-test-permission-cache-cleared-by-function ()
  "mcp-server-security-clear-permissions must empty the cache."
  (mcp-test-with-mock-server
   (puthash "op:data" t mcp-server-security--permission-cache)
   (puthash "op2:data" t mcp-server-security--permission-cache)
   (mcp-server-security-clear-permissions)
   (should (zerop (hash-table-count mcp-server-security--permission-cache)))))

(ert-deftest mcp-security-test-permission-cache-hit-skips-re-evaluation ()
  "A cached permission decision must be returned without re-evaluation."
  (mcp-test-with-mock-server
   ;; Pre-populate cache with a granted decision
   (puthash "cached-op:nil" t mcp-server-security--permission-cache)
   ;; Even though we pass a dangerous operation name, the cache wins
   (should (mcp-server-security-check-permission 'cached-op nil))))

;;; Tests for input validation

(ert-deftest mcp-security-test-validate-input-rejects-shell-metacharacters ()
  "Shell metacharacters in input must be rejected."
  (should-error (mcp-server-security-validate-input "test; rm -rf /"))
  (should-error (mcp-server-security-validate-input "cmd | grep secret"))
  (should-error (mcp-server-security-validate-input "$(whoami)"))
  (should-error (mcp-server-security-validate-input "`id`")))

(ert-deftest mcp-security-test-validate-input-allows-safe-strings ()
  "Safe, plain strings must pass validation."
  (should (mcp-server-security-validate-input "hello world"))
  (should (mcp-server-security-validate-input "a normal expression"))
  (should (mcp-server-security-validate-input "(+ 1 2 3)")))

(ert-deftest mcp-security-test-validate-input-rejects-dangerous-elisp ()
  "Inputs containing dangerous elisp call patterns must be rejected."
  (should-error (mcp-server-security-validate-input "(eval something)"))
  (should-error (mcp-server-security-validate-input "(shell-command \"ls\")"))
  (should-error (mcp-server-security-validate-input "(load \"/tmp/evil.el\")")))

(ert-deftest mcp-security-test-validate-input-rejects-overlong-input ()
  "Inputs exceeding maximum length must be rejected."
  (should-error (mcp-server-security-validate-input (make-string 10001 ?a))))

(ert-deftest mcp-security-test-validate-input-returns-input-on-success ()
  "Successful validation must return the original input unchanged."
  (let ((safe "a safe string"))
    (should (equal (mcp-server-security-validate-input safe) safe))))

;;; Tests for audit logging

(ert-deftest mcp-security-test-audit-log-records-events ()
  "Security events must be appended to the audit log with correct fields."
  (mcp-test-with-mock-server
   (let ((mcp-server-security--audit-log nil))
     (mcp-server-security--log-audit 'test-operation "test-data" t)
     (should (= (length mcp-server-security--audit-log) 1))
     (let ((entry (car mcp-server-security--audit-log)))
       (should (equal (alist-get 'operation entry) 'test-operation))
       (should (equal (alist-get 'data entry) "test-data"))
       (should (alist-get 'granted entry))
       (should (alist-get 'timestamp entry))))))

(ert-deftest mcp-security-test-audit-log-records-denials ()
  "Denied operations must also be recorded in the audit log."
  (mcp-test-with-mock-server
   (let ((mcp-server-security--audit-log nil))
     (mcp-server-security--log-audit 'blocked-op nil nil)
     (let ((entry (car mcp-server-security--audit-log)))
       (should (equal (alist-get 'operation entry) 'blocked-op))
       (should-not (alist-get 'granted entry))))))

(ert-deftest mcp-security-test-audit-log-capped-at-1000 ()
  "Audit log must not grow beyond 1000 entries."
  (mcp-test-with-mock-server
   (let ((mcp-server-security--audit-log nil))
     (dotimes (_ 1050)
       (mcp-server-security--log-audit 'test-op nil t))
     (should (<= (length mcp-server-security--audit-log) 1000)))))

;;; Tests for missing dangerous functions (quick-win gap #2 and #5)

(ert-deftest mcp-security-test-with-temp-file-blocked ()
  "with-temp-file must be in the dangerous functions list.
It writes a file via write-region internally, but the static checker
never sees write-region because it operates on unexpanded forms."
  (mcp-test-with-mock-server
   (let ((mcp-server-security-allowed-dangerous-functions nil)
         (mcp-server-security-prompt-for-permissions nil))
     (should-error
      (mcp-server-security--check-form-safety
       '(with-temp-file "/tmp/evil.sh" (insert "rm -rf /")))
      :type 'error))))

(ert-deftest mcp-security-test-write-file-blocked ()
  "write-file must be in the dangerous functions list."
  (mcp-test-with-mock-server
   (let ((mcp-server-security-allowed-dangerous-functions nil)
         (mcp-server-security-prompt-for-permissions nil))
     (should-error
      (mcp-server-security--check-form-safety '(write-file "/tmp/test"))
      :type 'error))))

(ert-deftest mcp-security-test-append-to-file-blocked ()
  "append-to-file must be in the dangerous functions list."
  (mcp-test-with-mock-server
   (let ((mcp-server-security-allowed-dangerous-functions nil)
         (mcp-server-security-prompt-for-permissions nil))
     (should-error
      (mcp-server-security--check-form-safety
       '(append-to-file (point-min) (point-max) "/tmp/test"))
      :type 'error))))

(ert-deftest mcp-security-test-make-network-process-blocked ()
  "make-network-process must be in the dangerous functions list."
  (mcp-test-with-mock-server
   (let ((mcp-server-security-allowed-dangerous-functions nil)
         (mcp-server-security-prompt-for-permissions nil))
     (should-error
      (mcp-server-security--check-form-safety
       '(make-network-process :name "test" :host "evil.com" :service 80))
      :type 'error))))

(ert-deftest mcp-security-test-open-network-stream-blocked ()
  "open-network-stream must be in the dangerous functions list."
  (mcp-test-with-mock-server
   (let ((mcp-server-security-allowed-dangerous-functions nil)
         (mcp-server-security-prompt-for-permissions nil))
     (should-error
      (mcp-server-security--check-form-safety
       '(open-network-stream "test" nil "evil.com" 80))
      :type 'error))))

(ert-deftest mcp-security-test-directory-files-blocked ()
  "directory-files must be in the dangerous functions list."
  (mcp-test-with-mock-server
   (let ((mcp-server-security-allowed-dangerous-functions nil)
         (mcp-server-security-prompt-for-permissions nil))
     (should-error
      (mcp-server-security--check-form-safety '(directory-files "~/.ssh/"))
      :type 'error))))

(ert-deftest mcp-security-test-directory-files-recursively-blocked ()
  "directory-files-recursively must be in the dangerous functions list."
  (mcp-test-with-mock-server
   (let ((mcp-server-security-allowed-dangerous-functions nil)
         (mcp-server-security-prompt-for-permissions nil))
     (should-error
      (mcp-server-security--check-form-safety
       '(directory-files-recursively "~/.ssh/" ".*"))
      :type 'error))))

(ert-deftest mcp-security-test-insert-file-contents-literally-blocked ()
  "insert-file-contents-literally must be in the dangerous functions list."
  (mcp-test-with-mock-server
   (let ((mcp-server-security-allowed-dangerous-functions nil)
         (mcp-server-security-prompt-for-permissions nil))
     (should-error
      (mcp-server-security--check-form-safety
       '(insert-file-contents-literally "~/.ssh/id_rsa"))
      :type 'error))))

;;; Tests for sensitive-file check on multi-argument file functions (gap #4)

(ert-deftest mcp-security-test-copy-file-sensitive-source-blocked ()
  "copy-file reading FROM a sensitive file must be blocked even when allowed."
  (mcp-test-with-mock-server
   (let ((mcp-server-security-dangerous-functions '(copy-file))
         (mcp-server-security-allowed-dangerous-functions '(copy-file))
         (mcp-server-security-sensitive-file-patterns '("~/.ssh/"))
         (mcp-server-security-prompt-for-permissions nil))
     (should-error
      (mcp-server-security--check-form-safety
       '(copy-file "~/.ssh/id_rsa" "/tmp/exfil"))
      :type 'error))))

(ert-deftest mcp-security-test-copy-file-sensitive-destination-blocked ()
  "copy-file writing TO a sensitive file must be blocked even when allowed."
  (mcp-test-with-mock-server
   (let ((mcp-server-security-dangerous-functions '(copy-file))
         (mcp-server-security-allowed-dangerous-functions '(copy-file))
         (mcp-server-security-sensitive-file-patterns '("~/.ssh/"))
         (mcp-server-security-prompt-for-permissions nil))
     (should-error
      (mcp-server-security--check-form-safety
       '(copy-file "/tmp/evil-key" "~/.ssh/authorized_keys"))
      :type 'error))))

(ert-deftest mcp-security-test-rename-file-sensitive-source-blocked ()
  "rename-file on a sensitive source must be blocked even when allowed."
  (mcp-test-with-mock-server
   (let ((mcp-server-security-dangerous-functions '(rename-file))
         (mcp-server-security-allowed-dangerous-functions '(rename-file))
         (mcp-server-security-sensitive-file-patterns '("~/.ssh/"))
         (mcp-server-security-prompt-for-permissions nil))
     (should-error
      (mcp-server-security--check-form-safety
       '(rename-file "~/.ssh/id_rsa" "/tmp/exfil"))
      :type 'error))))

(ert-deftest mcp-security-test-rename-file-sensitive-destination-blocked ()
  "rename-file to a sensitive destination must be blocked even when allowed."
  (mcp-test-with-mock-server
   (let ((mcp-server-security-dangerous-functions '(rename-file))
         (mcp-server-security-allowed-dangerous-functions '(rename-file))
         (mcp-server-security-sensitive-file-patterns '("~/.ssh/"))
         (mcp-server-security-prompt-for-permissions nil))
     (should-error
      (mcp-server-security--check-form-safety
       '(rename-file "/tmp/evil-key" "~/.ssh/authorized_keys"))
      :type 'error))))

(ert-deftest mcp-security-test-write-region-sensitive-destination-blocked ()
  "write-region targeting a sensitive file must be blocked even when allowed."
  (mcp-test-with-mock-server
   (let ((mcp-server-security-dangerous-functions '(write-region))
         (mcp-server-security-allowed-dangerous-functions '(write-region))
         (mcp-server-security-sensitive-file-patterns '("~/.ssh/"))
         (mcp-server-security-prompt-for-permissions nil))
     ;; write-region FILENAME is the 3rd positional argument
     (should-error
      (mcp-server-security--check-form-safety
       '(write-region (point-min) (point-max) "~/.ssh/authorized_keys"))
      :type 'error))))

(ert-deftest mcp-security-test-insert-file-contents-literally-sensitive-blocked ()
  "insert-file-contents-literally on a sensitive file must be blocked when allowed."
  (mcp-test-with-mock-server
   (let ((mcp-server-security-dangerous-functions '(insert-file-contents-literally))
         (mcp-server-security-allowed-dangerous-functions
          '(insert-file-contents-literally))
         (mcp-server-security-sensitive-file-patterns '("~/.ssh/"))
         (mcp-server-security-prompt-for-permissions nil))
     (should-error
      (mcp-server-security--check-form-safety
       '(insert-file-contents-literally "~/.ssh/id_rsa"))
      :type 'error))))

(ert-deftest mcp-security-test-write-file-sensitive-destination-blocked ()
  "write-file to a sensitive path must be blocked even when allowed."
  (mcp-test-with-mock-server
   (let ((mcp-server-security-dangerous-functions '(write-file))
         (mcp-server-security-allowed-dangerous-functions '(write-file))
         (mcp-server-security-sensitive-file-patterns '("~/.ssh/"))
         (mcp-server-security-prompt-for-permissions nil))
     (should-error
      (mcp-server-security--check-form-safety
       '(write-file "~/.ssh/authorized_keys"))
      :type 'error))))

(ert-deftest mcp-security-test-copy-file-safe-paths-allowed ()
  "copy-file between non-sensitive paths must be allowed when in the allowed list."
  (mcp-test-with-mock-server
   (let ((mcp-server-security-dangerous-functions '(copy-file))
         (mcp-server-security-allowed-dangerous-functions '(copy-file))
         (mcp-server-security-sensitive-file-patterns '("~/.ssh/"))
         (mcp-server-security-prompt-for-permissions nil))
     ;; Must not error - neither path is sensitive
     (mcp-server-security--check-form-safety
      '(copy-file "/tmp/src.txt" "/tmp/dst.txt")))))

;;; Tests for glob patterns in mcp-server-security-sensitive-file-patterns

(ert-deftest mcp-security-test-sensitive-file-glob-authinfo-star ()
  "~/.authinfo* must block all .authinfo variants.
This is the exact pattern used in issue #9. Without glob support,
expand-file-name produces a literal * and no variant is matched."
  (let ((mcp-server-security-sensitive-file-patterns '("~/.authinfo*"))
        (mcp-server-security-allowed-sensitive-files nil))
    (should (mcp-server-security--is-sensitive-file "~/.authinfo"))
    (should (mcp-server-security--is-sensitive-file "~/.authinfo.gpg"))
    (should (mcp-server-security--is-sensitive-file "~/.authinfo.enc"))
    (should (mcp-server-security--is-sensitive-file "~/.authinfo.gpg~"))
    ;; Unrelated file must not match
    (should-not (mcp-server-security--is-sensitive-file "~/.bashrc"))))

(ert-deftest mcp-security-test-sensitive-file-glob-netrc-star ()
  "~/.netrc* must block all .netrc variants."
  (let ((mcp-server-security-sensitive-file-patterns '("~/.netrc*"))
        (mcp-server-security-allowed-sensitive-files nil))
    (should (mcp-server-security--is-sensitive-file "~/.netrc"))
    (should (mcp-server-security--is-sensitive-file "~/.netrc.gpg"))
    (should (mcp-server-security--is-sensitive-file "~/.netrc.enc"))
    (should-not (mcp-server-security--is-sensitive-file "~/.bashrc"))))

(ert-deftest mcp-security-test-sensitive-file-glob-issue-9-full-config ()
  "Issue #9 exact config with glob patterns must block all sensitive variants."
  (let ((mcp-server-security-sensitive-file-patterns
         '("~/.authinfo*" "~/.netrc*" "~/.ssh/" "~/.gnupg/"))
        (mcp-server-security-allowed-sensitive-files nil))
    ;; authinfo variants
    (should (mcp-server-security--is-sensitive-file "~/.authinfo"))
    (should (mcp-server-security--is-sensitive-file "~/.authinfo.gpg"))
    (should (mcp-server-security--is-sensitive-file "~/.authinfo.enc"))
    ;; netrc variants
    (should (mcp-server-security--is-sensitive-file "~/.netrc"))
    (should (mcp-server-security--is-sensitive-file "~/.netrc.gpg"))
    ;; ssh and gnupg
    (should (mcp-server-security--is-sensitive-file "~/.ssh/id_rsa"))
    (should (mcp-server-security--is-sensitive-file "~/.gnupg/secring.gpg"))
    ;; safe files
    (should-not (mcp-server-security--is-sensitive-file "~/.bashrc"))
    (should-not (mcp-server-security--is-sensitive-file "/tmp/safe.txt"))))

(ert-deftest mcp-security-test-sensitive-file-glob-expanded-path-with-glob ()
  "Glob patterns must also match when an already-expanded path is tested."
  (let ((mcp-server-security-sensitive-file-patterns '("~/.authinfo*"))
        (mcp-server-security-allowed-sensitive-files nil))
    (should (mcp-server-security--is-sensitive-file
             (expand-file-name "~/.authinfo.gpg")))))

(provide 'test-mcp-security)
;;; test-mcp-security.el ends here
