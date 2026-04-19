;;; mcp-server-emacs-tools-org-update-node.el --- org-update-node MCP tool -*- lexical-binding: t; -*-

;;; Commentary:

;; Modify an existing heading using org's native APIs.

;;; Code:

(require 'mcp-server-tools)
(require 'mcp-server-emacs-tools-org-common)
(require 'org)
(require 'org-element)
(require 'json)

(defun mcp-server-emacs-tools-org-update-node--apply-body (new-body)
  "Replace the body of the heading at point with NEW-BODY.
The body is the text between the metadata (properties drawer,
SCHEDULED, DEADLINE, LOGBOOK) and the first child heading or end of
subtree.  Properties and metadata are preserved."
  (save-excursion
    (org-back-to-heading t)
    (org-end-of-meta-data t)
    (let ((body-start (point))
          (body-end (save-excursion
                      (if (outline-next-heading)
                          (point)
                        (point-max)))))
      (delete-region body-start body-end)
      (goto-char body-start)
      (insert new-body)
      (unless (bolp) (insert "\n")))))

(defun mcp-server-emacs-tools-org-update-node--handler (args)
  "Handle org-update-node tool call with ARGS."
  (condition-case err
      (let* ((file (alist-get 'file args))
             (_ (when file (mcp-server-emacs-tools-org-common--validate-path file)))
             (marker (mcp-server-emacs-tools-org-common--resolve-node args))
             (changed '()))
        (mcp-server-emacs-tools-org-common--with-node marker
          (let* ((title (alist-get 'title args))
                 (todo (alist-get 'todo_state args))
                 (priority (alist-get 'priority args))
                 (tags-present (assq 'tags args))
                 (tags (alist-get 'tags args))
                 (add-tags (alist-get 'add_tags args))
                 (remove-tags (alist-get 'remove_tags args))
                 (properties (alist-get 'properties args))
                 (remove-properties (alist-get 'remove_properties args))
                 (scheduled (mcp-server-emacs-tools-org-common--clearable-arg
                             args 'scheduled))
                 (deadline (mcp-server-emacs-tools-org-common--clearable-arg
                            args 'deadline))
                 (body (alist-get 'body args)))
            (when title
              (org-edit-headline title)
              (push "title" changed))
            (when todo
              (org-todo todo)
              (push "todo_state" changed))
            (when priority
              (org-priority (string-to-char priority))
              (push "priority" changed))
            (when (or tags-present add-tags remove-tags)
              (let* ((current (org-get-tags nil t))
                     (new-tags
                      (cond
                       ;; Presence of `tags' key (possibly empty) means
                       ;; "replace entire tag set", including clearing
                       ;; all tags when the value is an empty list.
                       (tags-present (append tags nil))
                       (t
                        (let ((acc current))
                          (dolist (t2 (append add-tags nil))
                            (unless (member t2 acc) (push t2 acc)))
                          (dolist (t2 (append remove-tags nil))
                            (setq acc (delete t2 acc)))
                          acc)))))
                (org-set-tags new-tags)
                (push "tags" changed)))
            (when properties
              (dolist (p properties)
                (org-set-property (format "%s" (car p)) (format "%s" (cdr p))))
              (push "properties" changed))
            (when remove-properties
              (dolist (k (append remove-properties nil))
                (org-delete-property k))
              (push "properties" changed))
            (unless (eq scheduled 'unset)
              (if (eq scheduled 'clear)
                  (org-schedule '(4))
                (org-schedule nil scheduled))
              (push "scheduled" changed))
            (unless (eq deadline 'unset)
              (if (eq deadline 'clear)
                  (org-deadline '(4))
                (org-deadline nil deadline))
              (push "deadline" changed))
            (when body
              (mcp-server-emacs-tools-org-update-node--apply-body body)
              (push "body" changed))))
        (let ((id (mcp-server-emacs-tools-org-common--promote-to-id marker)))
          (json-encode `((id . ,id)
                         (changed . ,(vconcat (nreverse changed)))))))
    (error (json-encode `((error . ,(error-message-string err)))))))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "org-update-node"
  :title "Update Org Node"
  :description "Modify an existing org heading.  Identify the heading by `id' (preferred) or `file'+`outline_path'.  Any subset of fields may be updated.  Pass null for `scheduled' or `deadline' to clear that field.  Before adding new tags, call org-list-tags to avoid near-duplicates (pluralization, casing, synonyms)."
  :input-schema '((type . "object")
                  (properties . ((id . ((type . "string")))
                                 (file . ((type . "string")))
                                 (outline_path . ((type . "array")
                                                  (items . ((type . "string")))))
                                 (title . ((type . "string")))
                                 (todo_state . ((type . "string")))
                                 (priority . ((type . "string")))
                                 (tags . ((type . "array")
                                          (items . ((type . "string")))))
                                 (add_tags . ((type . "array")
                                              (items . ((type . "string")))))
                                 (remove_tags . ((type . "array")
                                                 (items . ((type . "string")))))
                                 (properties . ((type . "object")))
                                 (remove_properties . ((type . "array")
                                                       (items . ((type . "string")))))
                                 (scheduled . ((type . ["string" "null"])
                                               (description . "Scheduled date/time, or null to clear")))
                                 (deadline . ((type . ["string" "null"])
                                              (description . "Deadline date/time, or null to clear")))
                                 (body . ((type . "string")))))
                  (required . []))
  :function #'mcp-server-emacs-tools-org-update-node--handler
  :annotations '((readOnlyHint . :false)
                 (destructiveHint . t)
                 (idempotentHint . :false)
                 (openWorldHint . :false))))

(provide 'mcp-server-emacs-tools-org-update-node)

;;; mcp-server-emacs-tools-org-update-node.el ends here
