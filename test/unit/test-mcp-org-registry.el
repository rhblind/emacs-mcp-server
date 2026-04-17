;;; test-mcp-org-registry.el --- Tests for org tools registry integration -*- lexical-binding: t; -*-
(require 'ert)
(require 'test-helpers)

(let* ((this-file (or load-file-name buffer-file-name))
       (tools-dir (and this-file
                       (expand-file-name "../../tools"
                                         (file-name-directory this-file)))))
  (when tools-dir (add-to-list 'load-path tools-dir)))

(require 'mcp-server-emacs-tools)

(ert-deftest mcp-test-org-registry-has-all-org-tools ()
  "The registry alist references all org-* tool features."
  (let ((names (mapcar #'car mcp-server-emacs-tools--available)))
    (dolist (expected '(org-agenda org-search org-get-node org-list-templates
                        org-list-tags org-capture org-update-node org-refile
                        org-archive org-clock))
      (should (memq expected names)))))

(ert-deftest mcp-test-org-registry-has-roam-tools ()
  "The registry alist references org-roam tool features."
  (let ((names (mapcar #'car mcp-server-emacs-tools--available)))
    (dolist (expected '(org-roam-search org-roam-get-node org-roam-capture))
      (should (memq expected names)))))

(ert-deftest mcp-test-org-registry-enabled-by-default ()
  "Tools are enabled under the default 'all value of the enabled var."
  (let ((mcp-server-emacs-tools-enabled 'all))
    (should (mcp-server-emacs-tools--tool-enabled-p "org-agenda"))
    (should (mcp-server-emacs-tools--tool-enabled-p "org-update-node"))))

(ert-deftest mcp-test-org-registry-can-be-disabled ()
  "A subset list disables non-listed tools."
  (let ((mcp-server-emacs-tools-enabled '(org-search)))
    (should (mcp-server-emacs-tools--tool-enabled-p "org-search"))
    (should-not (mcp-server-emacs-tools--tool-enabled-p "org-update-node"))))

(provide 'test-mcp-org-registry)
;;; test-mcp-org-registry.el ends here
