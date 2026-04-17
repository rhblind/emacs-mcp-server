;;; mcp-server-emacs-tools-org-clock.el --- org-clock MCP tool -*- lexical-binding: t; -*-

;;; Commentary:

;; Clock in, out, or cancel on org headings.

;;; Code:

(require 'mcp-server-tools)
(require 'mcp-server-emacs-tools-org-common)
(require 'org)
(require 'org-clock)
(require 'json)

(defun mcp-server-emacs-tools-org-clock--handler (args)
  "Handle org-clock tool call with ARGS."
  (condition-case err
      (let ((action (or (alist-get 'action args) (error "`action' is required"))))
        (pcase action
          ("in"
           (let* ((marker (mcp-server-emacs-tools-org-common--resolve-node args)))
             (with-current-buffer (marker-buffer marker)
               (save-excursion
                 (goto-char marker)
                 (org-clock-in)
                 (when mcp-server-emacs-tools-org-auto-save (save-buffer))))
             (let* ((id (alist-get 'id args))
                    (title (and (markerp marker)
                                (with-current-buffer (marker-buffer marker)
                                  (save-excursion
                                    (goto-char marker)
                                    (when (org-at-heading-p)
                                      (org-get-heading t t t t)))))))
               (json-encode `((action . "in")
                              (clocked_id . ,id)
                              (clocked_title . ,title))))))
          ("out"
           (let ((info (and (boundp 'org-clock-marker)
                            (markerp org-clock-marker)
                            (marker-position org-clock-marker))))
             (when info (org-clock-out))
             (json-encode `((action . "out")))))
          ("cancel"
           (when (and (boundp 'org-clock-marker)
                      (markerp org-clock-marker)
                      (marker-position org-clock-marker))
             (org-clock-cancel))
           (json-encode `((action . "cancel"))))
          (_ (error "Unknown action: %s" action))))
    (error (json-encode `((error . ,(error-message-string err)))))))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "org-clock"
  :title "Org Clock"
  :description "Clock in, out, or cancel.  Action `in' requires `id' or `file'+`outline_path'.  Actions `out' and `cancel' act on the currently clocked task."
  :input-schema '((type . "object")
                  (properties . ((action . ((type . "string")
                                            (enum . ("in" "out" "cancel"))))
                                 (id . ((type . "string")))
                                 (file . ((type . "string")))
                                 (outline_path . ((type . "array")
                                                  (items . ((type . "string")))))))
                  (required . ["action"]))
  :function #'mcp-server-emacs-tools-org-clock--handler
  :annotations '((readOnlyHint . :false)
                 (destructiveHint . t)
                 (idempotentHint . :false)
                 (openWorldHint . :false))))

(provide 'mcp-server-emacs-tools-org-clock)

;;; mcp-server-emacs-tools-org-clock.el ends here
