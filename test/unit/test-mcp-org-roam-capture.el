;;; test-mcp-org-roam-capture.el --- Tests for org-roam-capture -*- lexical-binding: t; -*-
(require 'ert)
(require 'test-helpers)
(require 'json)

(let* ((this-file (or load-file-name buffer-file-name))
       (tools-dir (and this-file
                       (expand-file-name "../../tools"
                                         (file-name-directory this-file)))))
  (when tools-dir (add-to-list 'load-path tools-dir)))

(require 'mcp-server-emacs-tools-org-roam-capture)

(ert-deftest mcp-test-org-roam-capture-registration-matches-availability ()
  "Tool registration matches org-roam availability."
  (if (featurep 'org-roam)
      (should (mcp-server-tools-exists-p "org-roam-capture"))
    (should-not (mcp-server-tools-exists-p "org-roam-capture"))))

(provide 'test-mcp-org-roam-capture)
;;; test-mcp-org-roam-capture.el ends here
