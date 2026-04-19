;;; mcp-server-emacs-tools-org-capture.el --- org-capture MCP tool -*- lexical-binding: t; -*-

;;; Commentary:

;; Create a new org entry.  Two modes: template (preferred) and direct.

;;; Code:

(require 'mcp-server-tools)
(require 'mcp-server-emacs-tools-org-common)
(require 'org)
(require 'org-capture)
(require 'org-id)
(require 'json)
(require 'seq)

(defun mcp-server-emacs-tools-org-capture--substitute-cursor (template content)
  "Return TEMPLATE with the first unescaped `%?' replaced by CONTENT.
MCP calls are non-interactive, so the `%?' cursor marker is pre-filled
with CONTENT before org-capture processes the template.  When TEMPLATE
contains no `%?', TEMPLATE is returned unchanged.  Respects org-capture
`%%' escaping: a literal `%?' in the output (written `%%?' in the
template) is not treated as the cursor marker."
  (if (not (and (stringp template) (stringp content)))
      template
    (let ((start 0) done result)
      (while (and (not done)
                  (string-match "\\(%%\\)\\|\\(%\\?\\)" template start))
        (cond
         ;; %% -> keep both characters and continue past them.
         ((match-beginning 1)
          (setq start (match-end 0)))
         ;; %? -> splice content in place of the match and stop.
         (t
          (setq result (concat (substring template 0 (match-beginning 2))
                               content
                               (substring template (match-end 2))))
          (setq done t))))
      (or result template))))

(defun mcp-server-emacs-tools-org-capture--template-mode (args)
  "Run org-capture with a template from ARGS.
Always finalizes synchronously; `immediate_finish' arg is accepted but ignored
because MCP calls are non-interactive.  The `content' arg is placed at the
template's `%?' cursor marker (when present) and also made available as `%i'."
  (let* ((key (alist-get 'template_key args))
         (content (alist-get 'content args))
         (entry (and key (assoc key org-capture-templates)))
         (modified-entry
          (and entry content (stringp (nth 4 entry))
               (append (seq-take entry 4)
                       (list (mcp-server-emacs-tools-org-capture--substitute-cursor
                              (nth 4 entry) content))
                       (nthcdr 5 entry))))
         (modified-templates
          (if modified-entry
              (cons modified-entry
                    (seq-remove (lambda (e) (equal (car-safe e) key))
                                org-capture-templates))
            org-capture-templates))
         (org-capture-templates modified-templates)
         (org-capture-initial (or content ""))
         (org-capture-templates-contexts nil))
    (save-window-excursion
      (org-capture nil key)
      (when (buffer-live-p (get-buffer "*Capture*"))
        (with-current-buffer "*Capture*" (org-capture-finalize))))
    (when (and (boundp 'org-capture-last-stored-marker)
               (markerp org-capture-last-stored-marker))
      (let ((marker org-capture-last-stored-marker))
        (with-current-buffer (marker-buffer marker)
          (save-excursion
            (goto-char marker)
            (let* ((id (mcp-server-emacs-tools-org-common--promote-to-id (point-marker)))
                   (file (buffer-file-name))
                   (olp (mcp-server-emacs-tools-org-common--compute-outline-path)))
              `((id . ,id) (file . ,file) (outline_path . ,(vconcat olp))))))))))

(defun mcp-server-emacs-tools-org-capture--direct-mode (args)
  "Create an entry directly from ARGS without a template."
  (let* ((file (or (alist-get 'file args) (error "Direct mode requires `file'")))
         (_ (mcp-server-emacs-tools-org-common--validate-path file))
         (outline-path (alist-get 'outline_path args))
         (title (or (alist-get 'title args) (error "`title' is required")))
         (body (alist-get 'body args))
         (todo-state (alist-get 'todo_state args))
         (priority (alist-get 'priority args))
         (tags (alist-get 'tags args))
         (properties (alist-get 'properties args))
         (scheduled (alist-get 'scheduled args))
         (deadline (alist-get 'deadline args))
         (buf (find-file-noselect file)))
    (with-current-buffer buf
      (org-with-wide-buffer
       (let (insert-marker level)
         (if outline-path
             (let ((parent-marker (mcp-server-emacs-tools-org-common--resolve-node
                                   `((file . ,file)
                                     (outline_path . ,outline-path)))))
               (goto-char parent-marker)
               (setq level (1+ (org-outline-level)))
               (org-end-of-subtree t t)
               (unless (bolp) (insert "\n"))
               (setq insert-marker (point-marker)))
           (goto-char (point-max))
           (unless (bolp) (insert "\n"))
           (setq insert-marker (point-marker))
           (setq level 1))
         (goto-char insert-marker)
         (insert (concat (make-string level ?*) " "
                         (when todo-state (concat todo-state " "))
                         title "\n"))
         (forward-line -1)
         (when tags (org-set-tags (append tags nil)))
         (when priority (org-priority (string-to-char priority)))
         (when properties
           (dolist (p properties)
             (org-set-property (format "%s" (car p)) (format "%s" (cdr p)))))
         (when scheduled (org-schedule nil scheduled))
         (when deadline (org-deadline nil deadline))
         (let ((id (mcp-server-emacs-tools-org-common--promote-to-id (point-marker))))
           (when body
             (org-end-of-meta-data t)
             (unless (bolp) (insert "\n"))
             (insert body)
             (unless (bolp) (insert "\n")))
           (when mcp-server-emacs-tools-org-auto-save (save-buffer))
           `((id . ,id)
             (file . ,file)
             (outline_path . ,(vconcat (mcp-server-emacs-tools-org-common--compute-outline-path))))))))))

(defun mcp-server-emacs-tools-org-capture--handler (args)
  "Handle org-capture tool call with ARGS."
  (condition-case err
      (let ((result
             (cond
              ((alist-get 'template_key args)
               (mcp-server-emacs-tools-org-capture--template-mode args))
              (t
               (mcp-server-emacs-tools-org-capture--direct-mode args)))))
        (json-encode result))
    (error (json-encode `((error . ,(error-message-string err)))))))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "org-capture"
  :title "Org Capture"
  :description "Create a new org entry.  Prefer `template_key' with an existing user template (see org-list-templates) because it respects user-configured targets and formatting.  Use direct mode (`file' + `title' + optional `outline_path'/`body'/`todo_state'/`tags') only when no template fits.  Before setting tags, check org-list-tags to avoid near-duplicates."
  :input-schema '((type . "object")
                  (properties . ((template_key . ((type . "string")))
                                 (content . ((type . "string")))
                                 (file . ((type . "string")))
                                 (outline_path . ((type . "array")
                                                  (items . ((type . "string")))))
                                 (title . ((type . "string")))
                                 (body . ((type . "string")))
                                 (todo_state . ((type . "string")))
                                 (priority . ((type . "string")))
                                 (tags . ((type . "array")
                                          (items . ((type . "string")))))
                                 (properties . ((type . "object")))
                                 (scheduled . ((type . "string")))
                                 (deadline . ((type . "string")))))
                  (required . []))
  :function #'mcp-server-emacs-tools-org-capture--handler
  :annotations '((readOnlyHint . :false)
                 (destructiveHint . t)
                 (idempotentHint . :false)
                 (openWorldHint . :false))))

(provide 'mcp-server-emacs-tools-org-capture)

;;; mcp-server-emacs-tools-org-capture.el ends here
