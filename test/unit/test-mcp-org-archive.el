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

(provide 'test-mcp-org-archive)
;;; test-mcp-org-archive.el ends here
