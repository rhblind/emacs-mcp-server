;;; test-mcp-org-refile.el --- Tests for org-refile -*- lexical-binding: t; -*-
(require 'ert)
(require 'test-helpers)
(require 'json)

(let* ((this-file (or load-file-name buffer-file-name))
       (tools-dir (and this-file
                       (expand-file-name "../../tools"
                                         (file-name-directory this-file)))))
  (when tools-dir (add-to-list 'load-path tools-dir)))

(require 'mcp-server-emacs-tools-org-refile)

(ert-deftest mcp-test-org-refile-moves-heading ()
  "refile moves source heading under target."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (mcp-server-emacs-tools-org-refile--handler
     '((source . ((id . "alpha-design-0001")))
       (target . ((id . "beta-root-0001")))))
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (search-forward "Project Beta")
      (search-forward "Design")
      (should t))))

(ert-deftest mcp-test-org-refile-registered ()
  "Tool is registered."
  (should (mcp-server-tools-exists-p "org-refile")))

(provide 'test-mcp-org-refile)
;;; test-mcp-org-refile.el ends here
