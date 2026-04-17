;;; mcp-server-emacs-tools-org-list-templates.el --- org-list-templates MCP tool -*- lexical-binding: t; -*-

;;; Commentary:

;; Return the user's configured capture templates so LLM clients can
;; choose an appropriate template_key for org-capture or org-roam-capture.

;;; Code:

(require 'mcp-server-tools)
(require 'org-capture)
(require 'json)

(defun mcp-server-emacs-tools-org-list-templates--template-to-alist (tmpl)
  "Convert TMPL (an entry from org-capture-templates) to an alist."
  (let* ((key (nth 0 tmpl))
         (description (nth 1 tmpl))
         (kind (nth 2 tmpl))
         (target (nth 3 tmpl))
         (target-type (when (listp target) (car target)))
         (target-file (when (and (listp target) (>= (length target) 2))
                        (let ((f (nth 1 target)))
                          (if (stringp f) f (format "%S" f)))))
         (target-heading (when (and (listp target)
                                    (memq target-type '(file+headline file+olp file+olp+datetree))
                                    (>= (length target) 3))
                           (let ((h (nth 2 target)))
                             (if (stringp h) h (format "%S" h))))))
    `((key . ,key)
      (description . ,description)
      (kind . ,(symbol-name (or kind 'entry)))
      (target_type . ,(when target-type (symbol-name target-type)))
      (target_file . ,target-file)
      (target_heading . ,target-heading))))

(defun mcp-server-emacs-tools-org-list-templates--handler (args)
  "Handle org-list-templates tool call with ARGS."
  (condition-case err
      (let* ((type (or (alist-get 'type args) "capture")))
        (pcase type
          ("capture"
           (let ((templates (mapcar #'mcp-server-emacs-tools-org-list-templates--template-to-alist
                                    (or org-capture-templates '()))))
             (json-encode `((templates . ,(vconcat templates))))))
          ("roam-capture"
           (if (featurep 'org-roam)
               (let ((templates (mapcar #'mcp-server-emacs-tools-org-list-templates--template-to-alist
                                        (or (and (boundp 'org-roam-capture-templates)
                                                 org-roam-capture-templates)
                                            '()))))
                 (json-encode `((templates . ,(vconcat templates)))))
             (json-encode `((error . "org-roam not installed")))))
          (_ (error "Unknown type: %s" type))))
    (error (json-encode `((error . ,(error-message-string err)))))))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "org-list-templates"
  :title "List Capture Templates"
  :description "Return the user's configured capture templates so you can pick the right template_key when calling org-capture or org-roam-capture."
  :input-schema '((type . "object")
                  (properties . ((type . ((type . "string")
                                          (enum . ("capture" "roam-capture"))))))
                  (required . []))
  :function #'mcp-server-emacs-tools-org-list-templates--handler
  :annotations '((readOnlyHint . t)
                 (destructiveHint . :false)
                 (idempotentHint . t)
                 (openWorldHint . :false))))

(provide 'mcp-server-emacs-tools-org-list-templates)

;;; mcp-server-emacs-tools-org-list-templates.el ends here
