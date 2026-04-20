;;; test-mcp-org-search.el --- Tests for org-search -*- lexical-binding: t; -*-
(require 'ert)
(require 'test-helpers)
(require 'json)
(require 'seq)

(let* ((this-file (or load-file-name buffer-file-name))
       (tools-dir (and this-file
                       (expand-file-name "../../tools"
                                         (file-name-directory this-file)))))
  (when tools-dir (add-to-list 'load-path tools-dir)))

(require 'mcp-server-emacs-tools-org-search)

(ert-deftest mcp-test-org-search-by-tag ()
  "org-search finds headings by tag in a single file."
  (mcp-test-with-org-fixture "sample-agenda.org" path
    (let* ((json (mcp-server-emacs-tools-org-search--handler
                  `((match . "work")
                    (scope . "file")
                    (files . [,path]))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json)))
           (results (alist-get 'results result)))
      (should (>= (length results) 2)))))

(ert-deftest mcp-test-org-search-by-todo-state ()
  "org-search finds headings by todo state."
  (mcp-test-with-org-fixture "sample-agenda.org" path
    (let* ((json (mcp-server-emacs-tools-org-search--handler
                  `((match . "/TODO")
                    (scope . "file")
                    (files . [,path]))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json)))
           (results (alist-get 'results result)))
      (should (>= (length results) 2))
      (dolist (r (append results nil))
        (should (equal (alist-get 'todo_state r) "TODO"))))))

(ert-deftest mcp-test-org-search-respects-limit ()
  "org-search caps results at the given limit."
  (mcp-test-with-org-fixture "sample-agenda.org" path
    (let* ((json (mcp-server-emacs-tools-org-search--handler
                  `((match . "/!")
                    (scope . "file")
                    (files . [,path])
                    (limit . 1))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json))))
      (should (= (length (alist-get 'results result)) 1))
      (should (eq (alist-get 'truncated result) t)))))

(ert-deftest mcp-test-org-search-registered ()
  "Tool is registered."
  (should (mcp-server-tools-exists-p "org-search")))

(ert-deftest mcp-test-org-search-returns-auto-created-ids ()
  "When auto-id promotes new IDs, results carry them.
Regression test: promote-to-id must run before serialization so that
newly-assigned IDs are visible in the search output."
  (mcp-test-with-org-fixture "sample-notes.org" path
    ;; `** Implementation' has no ID in the fixture.  `{Implementation}'
    ;; is org's heading-text regexp match.
    (let* ((mcp-server-emacs-tools-org-auto-id t)
           (json (mcp-server-emacs-tools-org-search--handler
                  `((match . "LEVEL=2")
                    (scope . "file")
                    (files . [,path]))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json)))
           (results (append (alist-get 'results result) nil))
           (impl (seq-find (lambda (r) (equal (alist-get 'title r) "Implementation"))
                           results)))
      (should impl)
      (should (stringp (alist-get 'id impl)))
      (should (> (length (alist-get 'id impl)) 0)))))

(provide 'test-mcp-org-search)
;;; test-mcp-org-search.el ends here
