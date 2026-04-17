;;; test-mcp-org-list-tags.el --- Tests for org-list-tags -*- lexical-binding: t; -*-
(require 'ert)
(require 'test-helpers)
(require 'json)

(let* ((this-file (or load-file-name buffer-file-name))
       (tools-dir (and this-file
                       (expand-file-name "../../tools"
                                         (file-name-directory this-file)))))
  (when tools-dir (add-to-list 'load-path tools-dir)))

(require 'mcp-server-emacs-tools-org-list-tags)

(ert-deftest mcp-test-org-list-tags-file-scope ()
  "list-tags returns tags with counts for scope=file."
  (mcp-test-with-org-fixture "sample-agenda.org" path
    (let* ((json (mcp-server-emacs-tools-org-list-tags--handler
                  `((scope . "file")
                    (files . [,path])
                    (include_configured . :false))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json)))
           (tags (append (alist-get 'tags result) nil))
           (work-entry (seq-find (lambda (entry) (equal (alist-get 'name entry) "work")) tags)))
      (should work-entry)
      (should (>= (alist-get 'count work-entry) 2)))))

(ert-deftest mcp-test-org-list-tags-includes-configured ()
  "list-tags includes configured tags when include_configured is t."
  (mcp-test-with-org-fixture "sample-agenda.org" path
    (let* ((org-tag-alist '(("planned") ("idea")))
           (json (mcp-server-emacs-tools-org-list-tags--handler
                  `((scope . "file")
                    (files . [,path])
                    (include_configured . t))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json)))
           (tags (append (alist-get 'tags result) nil))
           (planned (seq-find (lambda (entry) (equal (alist-get 'name entry) "planned")) tags)))
      (should planned)
      (should (= (alist-get 'count planned) 0)))))

(ert-deftest mcp-test-org-list-tags-registered ()
  "Tool is registered."
  (should (mcp-server-tools-exists-p "org-list-tags")))

(provide 'test-mcp-org-list-tags)
;;; test-mcp-org-list-tags.el ends here
