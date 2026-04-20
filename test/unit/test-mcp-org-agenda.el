;;; test-mcp-org-agenda.el --- Tests for org-agenda -*- lexical-binding: t; -*-
(require 'ert)
(require 'test-helpers)
(require 'json)
(require 'seq)

(let* ((this-file (or load-file-name buffer-file-name))
       (tools-dir (and this-file
                       (expand-file-name "../../tools"
                                         (file-name-directory this-file)))))
  (when tools-dir (add-to-list 'load-path tools-dir)))

(require 'mcp-server-emacs-tools-org-agenda)

(ert-deftest mcp-test-org-agenda-todo-view ()
  "org-agenda with view=todo returns TODO items."
  (mcp-test-with-org-fixture "sample-agenda.org" path
    (let* ((org-agenda-files (list path))
           (json (mcp-server-emacs-tools-org-agenda--handler
                  '((view . "todo"))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json)))
           (entries (alist-get 'entries result)))
      (should (>= (length entries) 2))
      (let ((count 0))
        (dolist (e (append entries nil))
          (when (member (alist-get 'todo_state e) '("TODO" "NEXT"))
            (cl-incf count)))
        (should (> count 0))))))

(ert-deftest mcp-test-org-agenda-tags-view ()
  "org-agenda with view=tags + match returns tagged entries."
  (mcp-test-with-org-fixture "sample-agenda.org" path
    (let* ((org-agenda-files (list path))
           (json (mcp-server-emacs-tools-org-agenda--handler
                  '((view . "tags") (match . "work"))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json))))
      (should (>= (length (alist-get 'entries result)) 1)))))

(ert-deftest mcp-test-org-agenda-registered ()
  "Tool is registered."
  (should (mcp-server-tools-exists-p "org-agenda")))

(ert-deftest mcp-test-org-agenda-returns-auto-created-ids ()
  "Agenda entries carry auto-created IDs for headings that lacked one.
Regression test: promote-to-id must run before serialization."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (let* ((mcp-server-emacs-tools-org-auto-id t)
           (org-agenda-files (list path))
           (json (mcp-server-emacs-tools-org-agenda--handler
                  '((view . "tags") (match . "LEVEL=2"))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json)))
           (entries (append (alist-get 'entries result) nil))
           (impl (seq-find (lambda (e) (equal (alist-get 'title e) "Implementation"))
                           entries)))
      (should impl)
      (should (stringp (alist-get 'id impl)))
      (should (> (length (alist-get 'id impl)) 0)))))

(ert-deftest mcp-test-org-agenda-rejects-files-outside-root ()
  "org-agenda rejects `files' outside allowed roots."
  (let* ((outside (make-temp-file "mcp-outside-" nil ".org"))
         (mcp-server-emacs-tools-org-allowed-roots (list "/nonexistent-root-xyz")))
    (unwind-protect
        (let* ((json (mcp-server-emacs-tools-org-agenda--handler
                      `((view . "todo") (files . [,outside]))))
               (result (let ((json-object-type 'alist)) (json-read-from-string json))))
          (should (alist-get 'error result)))
      (when (file-exists-p outside) (delete-file outside)))))

(provide 'test-mcp-org-agenda)
;;; test-mcp-org-agenda.el ends here
