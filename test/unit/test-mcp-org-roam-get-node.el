;;; test-mcp-org-roam-get-node.el --- Tests for org-roam-get-node -*- lexical-binding: t; -*-
(require 'ert)
(require 'test-helpers)
(require 'json)

(let* ((this-file (or load-file-name buffer-file-name))
       (tools-dir (and this-file
                       (expand-file-name "../../tools"
                                         (file-name-directory this-file)))))
  (when tools-dir (add-to-list 'load-path tools-dir)))

(require 'mcp-server-emacs-tools-org-roam-get-node)

(ert-deftest mcp-test-org-roam-get-node-registration-matches-availability ()
  "Tool registration matches org-roam availability."
  (if (featurep 'org-roam)
      (should (mcp-server-tools-exists-p "org-roam-get-node"))
    (should-not (mcp-server-tools-exists-p "org-roam-get-node"))))

(provide 'test-mcp-org-roam-get-node)
;;; test-mcp-org-roam-get-node.el ends here
