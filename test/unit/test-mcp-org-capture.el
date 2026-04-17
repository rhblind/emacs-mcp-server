;;; test-mcp-org-capture.el --- Tests for org-capture -*- lexical-binding: t; -*-
(require 'ert)
(require 'test-helpers)
(require 'json)

(let* ((this-file (or load-file-name buffer-file-name))
       (tools-dir (and this-file
                       (expand-file-name "../../tools"
                                         (file-name-directory this-file)))))
  (when tools-dir (add-to-list 'load-path tools-dir)))

(require 'mcp-server-emacs-tools-org-capture)

(ert-deftest mcp-test-org-capture-direct-mode ()
  "Direct mode appends a heading to the target file."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (let* ((json (mcp-server-emacs-tools-org-capture--handler
                  `((file . ,path)
                    (title . "New top-level task")
                    (body . "Body content.")
                    (todo_state . "TODO")
                    (tags . ["followup"]))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json))))
      (should (alist-get 'id result))
      (with-temp-buffer
        (insert-file-contents path)
        (should (string-match-p "TODO New top-level task" (buffer-string)))
        (should (string-match-p ":followup:" (buffer-string)))
        (should (string-match-p "Body content\\." (buffer-string)))))))

(ert-deftest mcp-test-org-capture-direct-under-parent ()
  "Direct mode under an outline_path appends as child."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (mcp-server-emacs-tools-org-capture--handler
     `((file . ,path)
       (outline_path . ["Project Alpha"])
       (title . "Child task")))
    (with-temp-buffer
      (insert-file-contents path)
      (should (string-match-p "\\*\\* Child task" (buffer-string))))))

(ert-deftest mcp-test-org-capture-rejects-path-outside-root ()
  "Direct mode refuses paths outside allowed roots."
  (let* ((outside (make-temp-file "mcp-outside-" nil ".org"))
         (mcp-server-emacs-tools-org-allowed-roots (list "/nonexistent-root")))
    (unwind-protect
        (let* ((json (mcp-server-emacs-tools-org-capture--handler
                      `((file . ,outside) (title . "x"))))
               (result (let ((json-object-type 'alist)) (json-read-from-string json))))
          (should (alist-get 'error result)))
      (when (file-exists-p outside) (delete-file outside)))))

(ert-deftest mcp-test-org-capture-registered ()
  "Tool is registered."
  (should (mcp-server-tools-exists-p "org-capture")))

(provide 'test-mcp-org-capture)
;;; test-mcp-org-capture.el ends here
