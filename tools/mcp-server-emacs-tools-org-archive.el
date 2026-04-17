;;; mcp-server-emacs-tools-org-archive.el --- org-archive MCP tool -*- lexical-binding: t; -*-

;;; Commentary:

;; Archive an org heading via `org-archive-subtree'.

;;; Code:

(require 'mcp-server-tools)
(require 'mcp-server-emacs-tools-org-common)
(require 'org)
(require 'org-archive)
(require 'json)

(defun mcp-server-emacs-tools-org-archive--handler (args)
  "Handle org-archive tool call with ARGS."
  (condition-case err
      (let* ((marker (mcp-server-emacs-tools-org-common--resolve-node args))
             (override (alist-get 'target args))
             (id (alist-get 'id args))
             (location nil))
        (with-current-buffer (marker-buffer marker)
          (save-excursion
            (goto-char marker)
            (let ((org-archive-location (or override org-archive-location)))
              (setq location org-archive-location)
              (org-archive-subtree))
            (when mcp-server-emacs-tools-org-auto-save (save-buffer))))
        (json-encode `((archived_id . ,id)
                       (archive_location . ,location))))
    (error (json-encode `((error . ,(error-message-string err)))))))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "org-archive"
  :title "Archive Org Heading"
  :description "Archive an org heading to its ARCHIVE target.  Respects `ARCHIVE' property on the node; optional `target' overrides the archive location (in `file::headline' format)."
  :input-schema '((type . "object")
                  (properties . ((id . ((type . "string")))
                                 (file . ((type . "string")))
                                 (outline_path . ((type . "array")
                                                  (items . ((type . "string")))))
                                 (target . ((type . "string")))))
                  (required . []))
  :function #'mcp-server-emacs-tools-org-archive--handler
  :annotations '((readOnlyHint . :false)
                 (destructiveHint . t)
                 (idempotentHint . :false)
                 (openWorldHint . :false))))

(provide 'mcp-server-emacs-tools-org-archive)

;;; mcp-server-emacs-tools-org-archive.el ends here
