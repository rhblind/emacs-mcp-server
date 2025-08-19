;;; test-mcp-security.el --- Tests for MCP Security Layer -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for security layer including permission management,
;; input validation, audit logging, and safe execution.

;;; Code:

(require 'ert)
(require 'test-helpers)

;;; Permission System Tests

(ert-deftest mcp-test-security-check-permission-allowed ()
  "Test permission check for explicitly allowed function."
  (mcp-test-with-mock-server
   (let ((mcp-server-security-allowed-dangerous-functions '(test-function)))
     (should (mcp-server-security-check-permission 'test-function)))))

(ert-deftest mcp-test-security-check-permission-dangerous ()
  "Test permission check for dangerous function requires prompt."
  (mcp-test-with-mock-server
   (mcp-test-with-permission-responses '(t)
     (should (mcp-server-security-check-permission 'delete-file)))))

(ert-deftest mcp-test-security-check-permission-denied ()
  "Test permission check for denied dangerous function."
  (mcp-test-with-mock-server
   (mcp-test-with-permission-responses '(nil)
     (should-not (mcp-server-security-check-permission 'delete-file)))))

(ert-deftest mcp-test-security-check-permission-cached ()
  "Test that permission decisions are cached."
  (mcp-test-with-mock-server
   (mcp-test-with-permission-responses '(t)
     (should (mcp-server-security-check-permission 'delete-file))
     (should (mcp-server-security-check-permission 'delete-file)))))

(ert-deftest mcp-test-security-grant-permission ()
  "Test explicitly granting permission."
  (mcp-test-with-mock-server
   (mcp-server-security-grant-permission 'test-function)
   (should (mcp-server-security-check-permission 'test-function))))

(ert-deftest mcp-test-security-deny-permission ()
  "Test explicitly denying permission."
  (mcp-test-with-mock-server
   (mcp-server-security-deny-permission 'test-function)
   (should-not (mcp-server-security-check-permission 'test-function))))

(ert-deftest mcp-test-security-clear-permissions ()
  "Test clearing permission cache."
  (mcp-test-with-mock-server
   (mcp-server-security-grant-permission 'test-function)
   (should (mcp-server-security-check-permission 'test-function))
   (mcp-server-security-clear-permissions)
   (mcp-test-with-permission-responses '(t)
     (should (mcp-server-security-check-permission 'test-function)))))

;;; Dangerous Function Detection Tests

(ert-deftest mcp-test-security-is-dangerous-operation ()
  "Test detection of dangerous operations."
  (should (mcp-server-security--is-dangerous-operation 'delete-file))
  (should (mcp-server-security--is-dangerous-operation 'shell-command))
  (should-not (mcp-server-security--is-dangerous-operation 'message))
  (should-not (mcp-server-security--is-dangerous-operation '+)))

(ert-deftest mcp-test-security-add-dangerous-function ()
  "Test adding function to dangerous list."
  (let ((original-list mcp-server-security-dangerous-functions))
    (unwind-protect
        (progn
          (mcp-server-security-add-dangerous-function 'custom-dangerous-func)
          (should (member 'custom-dangerous-func mcp-server-security-dangerous-functions)))
      (setq mcp-server-security-dangerous-functions original-list))))

(ert-deftest mcp-test-security-remove-dangerous-function ()
  "Test removing function from dangerous list."
  (let ((original-list mcp-server-security-dangerous-functions))
    (unwind-protect
        (progn
          (mcp-server-security-add-dangerous-function 'temp-dangerous-func)
          (should (member 'temp-dangerous-func mcp-server-security-dangerous-functions))
          (mcp-server-security-remove-dangerous-function 'temp-dangerous-func)
          (should-not (member 'temp-dangerous-func mcp-server-security-dangerous-functions)))
      (setq mcp-server-security-dangerous-functions original-list))))

;;; Sensitive File Detection Tests

(ert-deftest mcp-test-security-is-sensitive-file ()
  "Test detection of sensitive files."
  (should (mcp-server-security--is-sensitive-file "~/.ssh/id_rsa"))
  (should (mcp-server-security--is-sensitive-file "~/.authinfo"))
  (should (mcp-server-security--is-sensitive-file "/etc/passwd"))
  (should (mcp-server-security--is-sensitive-file "~/passwords.txt"))
  (should-not (mcp-server-security--is-sensitive-file "~/normal-file.txt"))
  (should-not (mcp-server-security--is-sensitive-file "/tmp/temp-file")))

(ert-deftest mcp-test-security-is-sensitive-buffer ()
  "Test detection of sensitive buffers."
  (should (mcp-server-security--is-sensitive-buffer "*scratch*"))
  (should (mcp-server-security--is-sensitive-buffer "secrets.txt"))
  (should (mcp-server-security--is-sensitive-buffer "credentials.json"))
  (should-not (mcp-server-security--is-sensitive-buffer "normal-buffer.el"))
  (should-not (mcp-server-security--is-sensitive-buffer "README.md")))

(ert-deftest mcp-test-security-contains-credentials ()
  "Test detection of credentials in content."
  (should (mcp-server-security--contains-credentials "password=secret123"))
  (should (mcp-server-security--contains-credentials "api_key=abc123def"))
  (should (mcp-server-security--contains-credentials "auth_token: xyz789"))
  (should (mcp-server-security--contains-credentials "secret_key = my-secret"))
  (should-not (mcp-server-security--contains-credentials "This is normal text"))
  (should-not (mcp-server-security--contains-credentials "function password() { return 'test'; }")))

;;; Input Validation Tests

(ert-deftest mcp-test-security-validate-input-valid ()
  "Test validation of valid input."
  (should (mcp-server-security-validate-input "normal string"))
  (should (mcp-server-security-validate-input 42))
  (should (mcp-server-security-validate-input '(list of symbols)))
  (should (mcp-server-security-validate-input t))
  (should (mcp-server-security-validate-input nil)))

(ert-deftest mcp-test-security-validate-input-suspicious ()
  "Test validation of suspicious input."
  (should-error (mcp-server-security-validate-input "rm -rf /"))
  (should-error (mcp-server-security-validate-input "$(malicious command)"))
  (should-error (mcp-server-security-validate-input "`dangerous`"))
  (should-error (mcp-server-security-validate-input "eval evil code")))

(ert-deftest mcp-test-security-sanitize-string ()
  "Test string sanitization."
  (should (equal (mcp-server-security-sanitize-string "normal text") "normal text"))
  (should (equal (mcp-server-security-sanitize-string "text with\nnewline") "text with newline"))
  (should (equal (mcp-server-security-sanitize-string "text\twith\ttabs") "text with tabs"))
  (should (equal (mcp-server-security-sanitize-string "text\r\nwith\r\ncarriage") "text with carriage")))

;;; Safe Evaluation Tests

(ert-deftest mcp-test-security-safe-eval-simple ()
  "Test safe evaluation of simple expressions."
  (mcp-test-with-mock-server
   (should (equal (mcp-server-security-safe-eval '(+ 1 2)) 3))
   (should (equal (mcp-server-security-safe-eval '(concat "hello" " world")) "hello world"))
   (should (equal (mcp-server-security-safe-eval '(length '(a b c))) 3))))

(ert-deftest mcp-test-security-safe-eval-dangerous ()
  "Test safe evaluation rejects dangerous forms."
  (mcp-test-with-mock-server
   (mcp-test-with-permission-responses '(nil)
     (should-error (mcp-server-security-safe-eval '(delete-file "/tmp/test"))))
   (mcp-test-with-permission-responses '(nil)
     (should-error (mcp-server-security-safe-eval '(shell-command "rm -rf /"))))
   (mcp-test-with-permission-responses '(nil)
     (should-error (mcp-server-security-safe-eval '(eval '(kill-emacs)))))))

(ert-deftest mcp-test-security-safe-eval-with-permission ()
  "Test safe evaluation with granted permission."
  (mcp-test-with-mock-server
   (mcp-test-with-temp-dir
    (let ((test-file (expand-file-name "test.txt" mcp-test-temp-dir)))
      (mcp-test-with-permission-responses '(t)
        (should-not (mcp-server-security-safe-eval `(delete-file ,test-file))))))))

(ert-deftest mcp-test-security-check-form-safety ()
  "Test form safety checking."
  (should-not (mcp-server-security--check-form-safety '(+ 1 2)))
  (should-not (mcp-server-security--check-form-safety '(message "hello")))
  (should (mcp-server-security--check-form-safety '(delete-file "/tmp/test")))
  (should (mcp-server-security--check-form-safety '(shell-command "ls")))
  (should (mcp-server-security--check-form-safety '(eval '(+ 1 2)))))

;;; Audit Logging Tests

(ert-deftest mcp-test-security-audit-logging ()
  "Test audit logging functionality."
  (mcp-test-with-mock-server
   (mcp-server-security--log-audit 'test-operation "test-data" t)
   (let ((log (mcp-server-security-get-audit-log 1)))
     (should (= (length log) 1))
     (let ((entry (car log)))
       (should (equal (alist-get 'operation entry) 'test-operation))
       (should (equal (alist-get 'data entry) "test-data"))
       (should (alist-get 'granted entry))
       (should (alist-get 'timestamp entry))))))

(ert-deftest mcp-test-security-audit-log-limit ()
  "Test audit log with limit."
  (mcp-test-with-mock-server
   (mcp-server-security--log-audit 'op1 "data1" t)
   (mcp-server-security--log-audit 'op2 "data2" nil)
   (mcp-server-security--log-audit 'op3 "data3" t)
   (let ((log (mcp-server-security-get-audit-log 2)))
     (should (= (length log) 2))
     (should (equal (alist-get 'operation (car log)) 'op3)))))

(ert-deftest mcp-test-security-clear-audit-log ()
  "Test clearing audit log."
  (mcp-test-with-mock-server
   (mcp-server-security--log-audit 'test-op "test-data" t)
   (should (> (length (mcp-server-security-get-audit-log)) 0))
   (mcp-server-security-clear-audit-log)
   (should (= (length (mcp-server-security-get-audit-log)) 0))))

;;; Execution Limits Tests

(ert-deftest mcp-test-security-execution-timeout ()
  "Test execution timeout protection."
  (mcp-test-with-mock-server
   (let ((mcp-server-security--max-execution-time 1))
     (should-error 
      (mcp-server-security--execute-with-limits
       (lambda () (sleep-for 2)))
      :type 'error))))

(ert-deftest mcp-test-security-execution-within-limits ()
  "Test execution within time limits."
  (mcp-test-with-mock-server
   (let ((mcp-server-security--max-execution-time 5))
     (should (equal
              (mcp-server-security--execute-with-limits
               (lambda () (+ 1 2 3)))
              6)))))

;;; Prompting Configuration Tests

(ert-deftest mcp-test-security-set-prompting ()
  "Test enabling/disabling security prompting."
  (let ((original mcp-server-security-prompt-for-permissions))
    (unwind-protect
        (progn
          (mcp-server-security-set-prompting nil)
          (should-not mcp-server-security-prompt-for-permissions)
          (mcp-server-security-set-prompting t)
          (should mcp-server-security-prompt-for-permissions))
      (setq mcp-server-security-prompt-for-permissions original))))

(ert-deftest mcp-test-security-no-prompting-allows-all ()
  "Test that disabling prompting allows all operations."
  (mcp-test-with-mock-server
   (let ((mcp-server-security-prompt-for-permissions nil))
     (should (mcp-server-security-check-permission 'delete-file))
     (should (mcp-server-security-check-permission 'shell-command)))))

;;; Integration Tests

(ert-deftest mcp-test-security-file-operation-flow ()
  "Test complete security flow for file operations."
  (mcp-test-with-mock-server
   (mcp-test-with-temp-dir
    (let ((test-file (expand-file-name "test.txt" mcp-test-temp-dir)))
      (with-temp-file test-file
        (insert "test content"))
      (mcp-test-with-permission-responses '(t)
        (should (mcp-server-security-check-permission 'find-file test-file)))
      (let ((log (mcp-server-security-get-audit-log 1)))
        (should (= (length log) 1))
        (should (alist-get 'granted (car log))))))))

(ert-deftest mcp-test-security-sensitive-content-detection ()
  "Test detection of sensitive content in files."
  (mcp-test-with-temp-dir
   (let ((sensitive-file (mcp-test-create-sensitive-file)))
     (with-temp-buffer
       (insert-file-contents sensitive-file)
       (should (mcp-server-security--contains-credentials (buffer-string)))))))

(provide 'test-mcp-security)
;;; test-mcp-security.el ends here