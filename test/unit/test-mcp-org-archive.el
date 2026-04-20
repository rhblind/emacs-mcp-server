;;; test-mcp-org-archive.el --- Tests for org-archive -*- lexical-binding: t; -*-
(require 'ert)
(require 'test-helpers)
(require 'json)

(let* ((this-file (or load-file-name buffer-file-name))
       (tools-dir (and this-file
                       (expand-file-name "../../tools"
                                         (file-name-directory this-file)))))
  (when tools-dir (add-to-list 'load-path tools-dir)))

(require 'mcp-server-emacs-tools-org-archive)

(ert-deftest mcp-test-org-archive-moves-to-archive ()
  "archive moves the subtree to its ARCHIVE target."
  (mcp-test-with-org-fixture "sample-agenda.org" path
    (let ((org-archive-location (concat path "_archive::* Archived")))
      (mcp-server-emacs-tools-org-archive--handler
       '((id . "agenda-task-0003")))
      (with-temp-buffer
        (insert-file-contents path)
        (should-not (string-match-p "Release v0.5.0" (buffer-string))))
      (let ((archive-file (concat path "_archive")))
        (when (file-exists-p archive-file)
          (with-temp-buffer
            (insert-file-contents archive-file)
            (should (string-match-p "Release v0.5.0" (buffer-string)))))))))

(ert-deftest mcp-test-org-archive-registered ()
  "Tool is registered."
  (should (mcp-server-tools-exists-p "org-archive")))

(ert-deftest mcp-test-org-archive-custom-target-override ()
  "archive with `target' arg uses that location, not the default."
  (mcp-test-with-org-fixture "sample-agenda.org" path
    (let* ((custom-archive (concat path "_custom"))
           (target (concat custom-archive "::* Custom Archive")))
      (let* ((json (mcp-server-emacs-tools-org-archive--handler
                    `((id . "agenda-task-0003")
                      (target . ,target))))
             (result (let ((json-object-type 'alist)) (json-read-from-string json))))
        (should (equal (alist-get 'archive_location result) target))
        (should (file-exists-p custom-archive))
        (with-temp-buffer
          (insert-file-contents custom-archive)
          (should (string-match-p "Release v0.5.0" (buffer-string))))
        ;; Source file no longer has the entry.
        (with-temp-buffer
          (insert-file-contents path)
          (should-not (string-match-p "Release v0.5.0" (buffer-string))))))))

(ert-deftest mcp-test-org-archive-missing-id ()
  "archive returns error JSON for unknown id."
  (mcp-test-with-org-fixture "sample-agenda.org" path
    (let* ((json (mcp-server-emacs-tools-org-archive--handler
                  '((id . "no-such-id"))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json))))
      (should (alist-get 'error result)))))

(ert-deftest mcp-test-org-archive-by-olp-returns-resolved-id ()
  "archive via file+outline_path populates `archived_id'.
Regression test: previously `archived_id' echoed `(alist-get 'id args)'
which was nil when the caller used `file'+`outline_path'."
  (mcp-test-with-org-fixture "sample-agenda.org" path
    (let* ((org-archive-location (concat path "_archive::* Archived"))
           (mcp-server-emacs-tools-org-auto-id t)
           (json (mcp-server-emacs-tools-org-archive--handler
                  `((file . ,path)
                    (outline_path . ["Buy groceries"]))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json))))
      (should (stringp (alist-get 'archived_id result)))
      (should (> (length (alist-get 'archived_id result)) 0)))))

(provide 'test-mcp-org-archive)
;;; test-mcp-org-archive.el ends here
