;;; test-mcp-org-common.el --- Tests for org-common -*- lexical-binding: t; -*-
(require 'ert)
(require 'test-helpers)

(let* ((this-file (or load-file-name buffer-file-name))
       (tools-dir (and this-file
                       (expand-file-name "../../tools"
                                         (file-name-directory this-file)))))
  (when tools-dir (add-to-list 'load-path tools-dir)))

(require 'mcp-server-emacs-tools-org-common)

(ert-deftest mcp-test-org-common-defcustoms-exist ()
  "Config defcustoms are defined with expected defaults."
  (should (boundp 'mcp-server-emacs-tools-org-auto-save))
  (should (eq mcp-server-emacs-tools-org-auto-save t))
  (should (boundp 'mcp-server-emacs-tools-org-auto-id))
  (should (eq mcp-server-emacs-tools-org-auto-id t))
  (should (boundp 'mcp-server-emacs-tools-org-allowed-roots))
  (should (null mcp-server-emacs-tools-org-allowed-roots))
  (should (boundp 'mcp-server-emacs-tools-org-max-body-bytes))
  (should (= mcp-server-emacs-tools-org-max-body-bytes 100000)))

(provide 'test-mcp-org-common)
;;; test-mcp-org-common.el ends here
