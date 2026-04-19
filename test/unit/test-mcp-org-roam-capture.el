;;; test-mcp-org-roam-capture.el --- Tests for org-roam-capture -*- lexical-binding: t; -*-
(require 'ert)
(require 'test-helpers)
(require 'json)

(let* ((this-file (or load-file-name buffer-file-name))
       (tools-dir (and this-file
                       (expand-file-name "../../tools"
                                         (file-name-directory this-file)))))
  (when tools-dir (add-to-list 'load-path tools-dir)))

(require 'mcp-server-emacs-tools-org-roam-capture)
(require 'mcp-server-emacs-tools-org-roam-search)

(defconst mcp-test-roam-available
  (and (>= emacs-major-version 29)
       (require 'org-roam nil t)))

(ert-deftest mcp-test-org-roam-capture-registration-matches-availability ()
  "Tool registration matches org-roam availability."
  (if (featurep 'org-roam)
      (should (mcp-server-tools-exists-p "org-roam-capture"))
    (should-not (mcp-server-tools-exists-p "org-roam-capture"))))

(ert-deftest mcp-test-org-roam-capture-direct-creates-node ()
  "Direct mode creates a node with given title and body."
  (skip-unless mcp-test-roam-available)
  (mcp-test-with-roam-fixture dir
    (let* ((json (mcp-server-emacs-tools-org-roam-capture--handler
                  '((title . "Fresh Node")
                    (body . "Some body content."))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json))))
      (should (alist-get 'id result))
      (should (alist-get 'file result))
      (should (equal (alist-get 'title result) "Fresh Node"))
      (let ((created-file (alist-get 'file result)))
        (should (file-exists-p created-file))
        (with-temp-buffer
          (insert-file-contents created-file)
          (should (string-match-p "Fresh Node" (buffer-string)))
          (should (string-match-p "Some body content" (buffer-string))))))))

(ert-deftest mcp-test-org-roam-capture-direct-with-aliases-and-refs ()
  "Direct mode preserves ALL aliases and refs (regression test for overwrite bug)."
  (skip-unless mcp-test-roam-available)
  (mcp-test-with-roam-fixture dir
    (let* ((json (mcp-server-emacs-tools-org-roam-capture--handler
                  '((title . "Multi")
                    (aliases . ["Alt1" "Alt2"])
                    (refs . ["https://example.com/x" "https://example.com/y"]))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json)))
           (file (alist-get 'file result)))
      (should file)
      (with-temp-buffer
        (insert-file-contents file)
        (let ((buf (buffer-string)))
          ;; Both aliases preserved (space-separated in the property).
          (should (string-match-p "Alt1" buf))
          (should (string-match-p "Alt2" buf))
          ;; Both refs preserved.
          (should (string-match-p "example.com/x" buf))
          (should (string-match-p "example.com/y" buf)))))))

(ert-deftest mcp-test-org-roam-capture-direct-with-tags ()
  "Direct mode writes file-level tags via `#+filetags:' on a title-only note.
Regression test: the previous implementation searched for `^\\*' and
called `org-set-tags' off-heading, which errors when the synthetic
template produced only a `#+title:' line."
  (skip-unless mcp-test-roam-available)
  (mcp-test-with-roam-fixture dir
    (let* ((json (mcp-server-emacs-tools-org-roam-capture--handler
                  '((title . "Tagged Node")
                    (tags . ["tagone" "tagtwo"]))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json)))
           (file (alist-get 'file result)))
      (should file)
      (with-temp-buffer
        (insert-file-contents file)
        (let ((buf (buffer-string)))
          (should (string-match-p "^#\\+filetags:.*:tagone:" buf))
          (should (string-match-p "^#\\+filetags:.*:tagtwo:" buf)))))))

(ert-deftest mcp-test-org-roam-capture-aliases-indexed-in-db ()
  "Direct mode's post-capture aliases appear in the roam DB.
Regression test: `org-roam-db-sync' now runs AFTER aliases/refs/filetags
have been written so `org-roam-search` can find them."
  (skip-unless mcp-test-roam-available)
  (mcp-test-with-roam-fixture dir
    (let* ((json (mcp-server-emacs-tools-org-roam-capture--handler
                  '((title . "Indexed Node")
                    (aliases . ["IndexedAlias"]))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json)))
           (id (alist-get 'id result)))
      (should (stringp id))
      ;; The alias must be visible to roam-search.
      (let* ((search-json (mcp-server-emacs-tools-org-roam-search--handler
                           '((query . "IndexedAlias"))))
             (search-result (let ((json-object-type 'alist))
                              (json-read-from-string search-json)))
             (results (append (alist-get 'results search-result) nil)))
        (should (seq-find (lambda (r) (equal (alist-get 'id r) id)) results))))))

(ert-deftest mcp-test-org-roam-capture-returns-created-id-not-title-collision ()
  "When another node shares the title, we return the one we created.
Regression test: the previous title-lookup could return the wrong node."
  (skip-unless mcp-test-roam-available)
  (mcp-test-with-roam-fixture dir
    ;; The fixture already has \"Concept A\"; create a second \"Concept A\"
    ;; and check the returned id is NOT the pre-existing one.
    (let* ((pre-existing-id "roam-concept-a-0001")
           (json (mcp-server-emacs-tools-org-roam-capture--handler
                  '((title . "Concept A"))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json)))
           (new-id (alist-get 'id result)))
      (should (stringp new-id))
      (should-not (equal new-id pre-existing-id)))))

(provide 'test-mcp-org-roam-capture)
;;; test-mcp-org-roam-capture.el ends here
