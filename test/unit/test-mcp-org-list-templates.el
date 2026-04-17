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
  "list-templates roam type returns error when roam is absent."
  (cl-letf (((symbol-function 'featurep)
             (lambda (feat &rest _) (not (eq feat 'org-roam)))))
    (let* ((json (mcp-server-emacs-tools-org-list-templates--handler
                  '((type . "roam-capture"))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json))))
      (should (alist-get 'error result)))))

(ert-deftest mcp-test-org-list-templates-registered ()
  "Tool is registered."
  (should (mcp-server-tools-exists-p "org-list-templates")))

(provide 'test-mcp-org-list-templates)
;;; test-mcp-org-list-templates.el ends here
