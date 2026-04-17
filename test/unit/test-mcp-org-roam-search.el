;;; test-mcp-org-roam-search.el --- Tests for org-roam-search -*- lexical-binding: t; -*-
(require 'ert)
(require 'test-helpers)
(require 'json)

(let* ((this-file (or load-file-name buffer-file-name))
       (tools-dir (and this-file
                       (expand-file-name "../../tools"
                                         (file-name-directory this-file)))))
  (when tools-dir (add-to-list 'load-path tools-dir)))

(require 'mcp-server-emacs-tools-org-roam-search)

(ert-deftest mcp-test-org-roam-search-absent-when-roam-missing ()
  "Tool does not register when org-roam is missing."
  (unless (featurep 'org-roam)
    (should-not (mcp-server-tools-exists-p "org-roam-search"))))

(ert-deftest mcp-test-org-roam-search-present-when-roam-loaded ()
  "Tool registers when org-roam is loaded."
  (when (featurep 'org-roam)
    (should (mcp-server-tools-exists-p "org-roam-search"))))

(provide 'test-mcp-org-roam-search)
;;; test-mcp-org-roam-search.el ends here
