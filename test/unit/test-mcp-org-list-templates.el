;;; test-mcp-org-list-templates.el --- Tests for org-list-templates -*- lexical-binding: t; -*-
(require 'ert)
(require 'test-helpers)
(require 'json)

(let* ((this-file (or load-file-name buffer-file-name))
       (tools-dir (and this-file
                       (expand-file-name "../../tools"
                                         (file-name-directory this-file)))))
  (when tools-dir (add-to-list 'load-path tools-dir)))

(require 'mcp-server-emacs-tools-org-list-templates)

(ert-deftest mcp-test-org-list-templates-capture ()
  "list-templates returns configured capture templates."
  (let ((org-capture-templates
         '(("t" "Todo" entry (file "/tmp/inbox.org") "* TODO %?" :immediate-finish t)
           ("j" "Journal" entry (file+datetree "/tmp/journal.org") "* %?"))))
    (let* ((json (mcp-server-emacs-tools-org-list-templates--handler
                  '((type . "capture"))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json)))
           (templates (alist-get 'templates result)))
      (should (= (length templates) 2))
      (let ((first (aref templates 0)))
        (should (equal (alist-get 'key first) "t"))
        (should (equal (alist-get 'description first) "Todo"))))))

(ert-deftest mcp-test-org-list-templates-roam-without-roam ()
  "list-templates roam type returns error when roam is absent.
The handler uses `(require 'org-roam nil t)`; mock that to return nil
for `org-roam' regardless of whether the package is installed."
  (cl-letf* ((orig-require (symbol-function 'require))
             ((symbol-function 'require)
              (lambda (feat &optional filename noerror)
                (if (eq feat 'org-roam) nil
                  (funcall orig-require feat filename noerror)))))
    (let* ((json (mcp-server-emacs-tools-org-list-templates--handler
                  '((type . "roam-capture"))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json))))
      (should (alist-get 'error result)))))

(ert-deftest mcp-test-org-list-templates-registered ()
  "Tool is registered."
  (should (mcp-server-tools-exists-p "org-list-templates")))

(ert-deftest mcp-test-org-list-templates-file-headline ()
  "Serializes a file+headline target correctly."
  (let ((org-capture-templates
         '(("h" "Heading" entry (file+headline "/tmp/notes.org" "Inbox") "* %?"))))
    (let* ((json (mcp-server-emacs-tools-org-list-templates--handler
                  '((type . "capture"))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json)))
           (tmpl (aref (alist-get 'templates result) 0)))
      (should (equal (alist-get 'target_type tmpl) "file+headline"))
      (should (equal (alist-get 'target_file tmpl) "/tmp/notes.org"))
      (should (equal (alist-get 'target_heading tmpl) "Inbox")))))

(ert-deftest mcp-test-org-list-templates-file-olp ()
  "Serializes a file+olp target (outline path as list)."
  (let ((org-capture-templates
         '(("o" "OLP" entry (file+olp "/tmp/notes.org" "Projects" "Active") "* %?"))))
    (let* ((json (mcp-server-emacs-tools-org-list-templates--handler
                  '((type . "capture"))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json)))
           (tmpl (aref (alist-get 'templates result) 0)))
      (should (equal (alist-get 'target_type tmpl) "file+olp"))
      (should (equal (alist-get 'target_file tmpl) "/tmp/notes.org")))))

(ert-deftest mcp-test-org-list-templates-file-datetree ()
  "Serializes a file+datetree target."
  (let ((org-capture-templates
         '(("j" "Journal" entry (file+datetree "/tmp/journal.org") "* %?"))))
    (let* ((json (mcp-server-emacs-tools-org-list-templates--handler
                  '((type . "capture"))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json)))
           (tmpl (aref (alist-get 'templates result) 0)))
      (should (equal (alist-get 'target_type tmpl) "file+datetree"))
      (should (equal (alist-get 'target_file tmpl) "/tmp/journal.org")))))

(ert-deftest mcp-test-org-list-templates-empty ()
  "When no templates configured, returns an empty templates array."
  (let ((org-capture-templates nil))
    (let* ((json (mcp-server-emacs-tools-org-list-templates--handler
                  '((type . "capture"))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json))))
      (should (vectorp (alist-get 'templates result)))
      (should (= (length (alist-get 'templates result)) 0)))))

(provide 'test-mcp-org-list-templates)
;;; test-mcp-org-list-templates.el ends here
