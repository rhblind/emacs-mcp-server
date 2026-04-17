;;; mcp-server-emacs-tools-org-refile.el --- org-refile MCP tool -*- lexical-binding: t; -*-

;;; Commentary:

;; Move an org heading under another heading using `org-refile'.

;;; Code:

(require 'mcp-server-tools)
(require 'mcp-server-emacs-tools-org-common)
(require 'org)
(require 'org-refile)
(require 'json)

(defun mcp-server-emacs-tools-org-refile--handler (args)
  "Handle org-refile tool call with ARGS."
  (condition-case err
      (let* ((source (or (alist-get 'source args) (error "`source' is required")))
             (target (or (alist-get 'target args) (error "`target' is required")))
             (source-marker (mcp-server-emacs-tools-org-common--resolve-node source))
             (target-marker (mcp-server-emacs-tools-org-common--resolve-node target))
             (target-buf (marker-buffer target-marker))
             (target-file (buffer-file-name target-buf))
             target-heading)
        (with-current-buffer target-buf
          (goto-char target-marker)
          (setq target-heading (when (org-at-heading-p) (org-get-heading t t t t))))
        (with-current-buffer (marker-buffer source-marker)
          (save-excursion
            (goto-char source-marker)
            (let ((org-refile-keep nil))
              (org-refile nil nil (list target-heading target-file nil target-marker)))
            (when mcp-server-emacs-tools-org-auto-save (save-buffer))))
        (with-current-buffer target-buf
          (when mcp-server-emacs-tools-org-auto-save (save-buffer)))
        (let* ((new-marker
                (org-id-find (or (alist-get 'id source) nil) 'marker))
               (new-olp (when (markerp new-marker)
                          (with-current-buffer (marker-buffer new-marker)
                            (save-excursion
                              (goto-char new-marker)
                              (mcp-server-emacs-tools-org-common--compute-outline-path))))))
          (json-encode `((id . ,(alist-get 'id source))
                         (new_file . ,target-file)
                         (new_outline_path . ,(vconcat (or new-olp '())))))))
    (error (json-encode `((error . ,(error-message-string err)))))))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "org-refile"
  :title "Refile Org Heading"
  :description "Move an org heading under another heading.  Source and target each accept `id' (preferred) or `file'+`outline_path'."
  :input-schema '((type . "object")
                  (properties . ((source . ((type . "object")))
                                 (target . ((type . "object")))))
                  (required . ["source" "target"]))
  :function #'mcp-server-emacs-tools-org-refile--handler
  :annotations '((readOnlyHint . :false)
                 (destructiveHint . t)
                 (idempotentHint . :false)
                 (openWorldHint . :false))))

(provide 'mcp-server-emacs-tools-org-refile)

;;; mcp-server-emacs-tools-org-refile.el ends here
