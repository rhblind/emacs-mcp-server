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

(ert-deftest mcp-test-org-list-tags-directory-scope ()
  "`scope'='directory' scans all .org files beneath the directory."
  (mcp-test-with-org-fixture "sample-agenda.org" path
    ;; The fixture's parent directory is the tmp dir; allowed-roots is
    ;; scoped to that tmp dir by the macro.
    (let* ((dir (file-name-directory path))
           (json (mcp-server-emacs-tools-org-list-tags--handler
                  `((scope . "directory")
                    (directory . ,dir)
                    (include_configured . :false))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json)))
           (tags (append (alist-get 'tags result) nil))
           (names (mapcar (lambda (entry) (alist-get 'name entry)) tags)))
      (should (member "work" names))
      (should (member "home" names))
      (should (member "release" names)))))

(ert-deftest mcp-test-org-list-tags-limit-caps-results ()
  "`limit' caps the returned tag list."
  (mcp-test-with-org-fixture "sample-agenda.org" path
    (let* ((json (mcp-server-emacs-tools-org-list-tags--handler
                  `((scope . "file")
                    (files . [,path])
                    (include_configured . :false)
                    (limit . 1))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json)))
           (tags (alist-get 'tags result)))
      (should (= (length tags) 1)))))

(ert-deftest mcp-test-org-list-tags-rejects-directory-outside-root ()
  "`scope'='directory' refuses paths outside allowed roots."
  (let ((mcp-server-emacs-tools-org-allowed-roots (list "/nonexistent-root-list-tags")))
    (let* ((json (mcp-server-emacs-tools-org-list-tags--handler
                  '((scope . "directory")
                    (directory . "/tmp"))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json))))
      (should (alist-get 'error result)))))

(provide 'test-mcp-org-list-tags)
;;; test-mcp-org-list-tags.el ends here
