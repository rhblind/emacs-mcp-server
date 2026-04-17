;;; mcp-server-emacs-tools-org-roam-capture.el --- org-roam-capture MCP tool -*- lexical-binding: t; -*-

;;; Commentary:

;; Create a new org-roam node.  Template mode preferred; direct mode is
;; the fallback.

;;; Code:

(require 'mcp-server-tools)
(require 'mcp-server-emacs-tools-org-common)
(require 'json)

(declare-function org-roam-capture- "org-roam-capture"
                  (&key keys node templates info props))
(declare-function org-roam-node-create "org-roam-node" (&rest slots))
(declare-function org-roam-db-query "org-roam-db" (sql &rest args))

(defun mcp-server-emacs-tools-org-roam-capture--direct (args)
  "Create a new roam node directly from ARGS without a template."
  (let* ((title (or (alist-get 'title args) (error "`title' is required")))
         (body (alist-get 'body args))
         (tags (alist-get 'tags args))
         (aliases (alist-get 'aliases args))
         (refs (alist-get 'refs args))
         (node (org-roam-node-create :title title))
         (synthetic-template
          `("x" "direct" plain ,(or body "")
            :target (file+head "${slug}.org" "#+title: ${title}\n")
            :immediate-finish t :unnarrowed t)))
    (org-roam-capture- :node node :templates (list synthetic-template))
    (let ((rows (org-roam-db-query
                 [:select [id file] :from nodes :where (= title $s1)]
                 title)))
      (when rows
        (let* ((row (car rows)) (id (nth 0 row)) (file (nth 1 row)))
          (when (or tags aliases refs)
            (with-current-buffer (find-file-noselect file)
              (save-excursion
                (goto-char (point-min))
                (dolist (alias (append aliases nil))
                  (org-set-property "ROAM_ALIASES" alias))
                (dolist (ref (append refs nil))
                  (org-set-property "ROAM_REFS" ref))
                (when tags
                  (when (re-search-forward "^\\*" nil t) (beginning-of-line))
                  (org-set-tags (append tags nil)))
                (when mcp-server-emacs-tools-org-auto-save (save-buffer)))))
          `((id . ,id) (file . ,file) (title . ,title)))))))

(defun mcp-server-emacs-tools-org-roam-capture--template (args)
  "Run a roam capture with a user template."
  (let* ((key (alist-get 'template_key args))
         (title (or (alist-get 'title args) (error "`title' is required")))
         (content (alist-get 'content args))
         (org-capture-initial (or content ""))
         (node (org-roam-node-create :title title)))
    (org-roam-capture- :keys key :node node)
    (let ((rows (org-roam-db-query
                 [:select [id file] :from nodes :where (= title $s1)]
                 title)))
      (when rows
        (let* ((row (car rows)) (id (nth 0 row)) (file (nth 1 row)))
          `((id . ,id) (file . ,file) (title . ,title)))))))

(defun mcp-server-emacs-tools-org-roam-capture--handler (args)
  "Handle org-roam-capture tool call with ARGS."
  (condition-case err
      (let ((result (if (alist-get 'template_key args)
                        (mcp-server-emacs-tools-org-roam-capture--template args)
                      (mcp-server-emacs-tools-org-roam-capture--direct args))))
        (json-encode result))
    (error (json-encode `((error . ,(error-message-string err)))))))

(when (require 'org-roam nil t)
  (mcp-server-register-tool
   (make-mcp-server-tool
    :name "org-roam-capture"
    :title "Create Org-Roam Node"
    :description "Create a new org-roam node.  Prefer `template_key' from the user's `org-roam-capture-templates' (see org-list-templates with type=roam-capture).  Direct mode supplies `title' + optional `body'/`tags'/`aliases'/`refs'."
    :input-schema '((type . "object")
                    (properties . ((template_key . ((type . "string")))
                                   (title . ((type . "string")))
                                   (content . ((type . "string")))
                                   (file . ((type . "string")))
                                   (body . ((type . "string")))
                                   (tags . ((type . "array")
                                            (items . ((type . "string")))))
                                   (aliases . ((type . "array")
                                               (items . ((type . "string")))))
                                   (refs . ((type . "array")
                                            (items . ((type . "string")))))
                                   (properties . ((type . "object")))))
                    (required . ["title"]))
    :function #'mcp-server-emacs-tools-org-roam-capture--handler
    :annotations '((readOnlyHint . :false)
                   (destructiveHint . t)
                   (idempotentHint . :false)
                   (openWorldHint . :false)))))

(provide 'mcp-server-emacs-tools-org-roam-capture)

;;; mcp-server-emacs-tools-org-roam-capture.el ends here
