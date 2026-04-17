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

(ert-deftest mcp-test-org-common-resolve-by-id ()
  "resolve-node finds a node by ID."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (let ((marker (mcp-server-emacs-tools-org-common--resolve-node
                   '((id . "alpha-design-0001")))))
      (should (markerp marker))
      (with-current-buffer (marker-buffer marker)
        (goto-char marker)
        (should (equal (org-get-heading t t t t) "Design"))))))

(ert-deftest mcp-test-org-common-resolve-by-olp ()
  "resolve-node finds a node by file + outline_path."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (let ((marker (mcp-server-emacs-tools-org-common--resolve-node
                   `((file . ,path)
                     (outline_path . ["Project Alpha" "Implementation"])))))
      (should (markerp marker))
      (with-current-buffer (marker-buffer marker)
        (goto-char marker)
        (should (equal (org-get-heading t t t t) "Implementation"))))))

(ert-deftest mcp-test-org-common-resolve-missing-id ()
  "resolve-node errors on unknown id."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (should-error
     (mcp-server-emacs-tools-org-common--resolve-node
      '((id . "does-not-exist"))))))

(ert-deftest mcp-test-org-common-resolve-missing-olp ()
  "resolve-node errors on unknown outline path."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (should-error
     (mcp-server-emacs-tools-org-common--resolve-node
      `((file . ,path)
        (outline_path . ["Project Alpha" "Nonexistent"]))))))

(ert-deftest mcp-test-org-common-resolve-requires-reference ()
  "resolve-node errors if neither id nor file is given."
  (should-error
   (mcp-server-emacs-tools-org-common--resolve-node '())))

(provide 'test-mcp-org-common)
;;; test-mcp-org-common.el ends here
