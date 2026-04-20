;;; mcp-server-emacs-tools-org-get-node.el --- org-get-node MCP tool -*- lexical-binding: t; -*-

;;; Commentary:

;; Fetch full content of a single heading or file.

;;; Code:

(require 'mcp-server-tools)
(require 'mcp-server-emacs-tools-org-common)
(require 'json)

(defun mcp-server-emacs-tools-org-get-node--handler (args)
  "Handle org-get-node tool call with ARGS."
  (condition-case err
      (let* ((include-body (mcp-server-emacs-tools-org-common--bool-arg
                            args 'include_body t))
             (include-children (mcp-server-emacs-tools-org-common--bool-arg
                                args 'include_children nil))
             (file (alist-get 'file args))
             (marker (progn
                       (when file
                         (mcp-server-emacs-tools-org-common--validate-path file))
                       (mcp-server-emacs-tools-org-common--resolve-node args))))
        ;; Promote to ID before serialization so the returned alist carries
        ;; the newly-assigned ID (when auto-id is on).
        (when (and mcp-server-emacs-tools-org-auto-id (not (alist-get 'id args)))
          (mcp-server-emacs-tools-org-common--promote-to-id marker))
        (let ((alist (mcp-server-emacs-tools-org-common--node-to-alist
                      marker :include-body include-body)))
          (when include-children
            (setq alist
                  (append alist
                          `((children . ,(mcp-server-emacs-tools-org-get-node--children marker))))))
          (json-encode alist)))
    (error (json-encode `((error . ,(error-message-string err)))))))

(defun mcp-server-emacs-tools-org-get-node--children (marker)
  "Return a vector of child-heading alists for node at MARKER."
  (with-current-buffer (marker-buffer marker)
    (org-with-wide-buffer
     (goto-char marker)
     (let ((children '()))
       (when (org-at-heading-p)
         (let ((level (org-outline-level)))
           (save-excursion
             (org-map-tree
              (lambda ()
                (when (= (org-outline-level) (1+ level))
                  (push (mcp-server-emacs-tools-org-common--node-to-alist
                         (point-marker) :include-body nil)
                        children)))))))
       (vconcat (nreverse children))))))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "org-get-node"
  :title "Get Org Node"
  :description "Fetch full content of an org heading or file.  Provide either `id' (preferred), or `file' plus optional `outline_path'.  File alone returns that file's pre-heading content.  When `mcp-server-emacs-tools-org-auto-id' is non-nil (default), a previously un-IDed heading is assigned an ID so later calls can reference it; set the defcustom to nil to disable that side effect."
  :input-schema '((type . "object")
                  (properties . ((id . ((type . "string")
                                        (description . "Org ID of the node (preferred)")))
                                 (file . ((type . "string")
                                          (description . "Absolute path to an org file")))
                                 (outline_path . ((type . "array")
                                                  (items . ((type . "string")))
                                                  (description . "Outline path under file")))
                                 (include_body . ((type . "boolean")
                                                  (description . "Return body content (default true)")))
                                 (include_children . ((type . "boolean")
                                                      (description . "Return child headings (default false)")))))
                  (required . []))
  :function #'mcp-server-emacs-tools-org-get-node--handler
  :annotations '((readOnlyHint . :false)
                 (destructiveHint . :false)
                 (idempotentHint . t)
                 (openWorldHint . :false))))

(provide 'mcp-server-emacs-tools-org-get-node)

;;; mcp-server-emacs-tools-org-get-node.el ends here
