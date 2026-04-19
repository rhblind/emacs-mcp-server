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
(declare-function org-roam-db-sync "org-roam-db" ())

(defun mcp-server-emacs-tools-org-roam-capture--last-captured ()
  "Return (ID . FILE) for the node most recently created by `org-capture'.
Uses `org-capture-last-stored-marker' so we locate the exact node
`org-roam-capture-' created, rather than querying the DB by title
(titles are not unique in roam).  Returns nil when the marker is
unset or the ID can't be read from the captured buffer."
  (when (and (boundp 'org-capture-last-stored-marker)
             (markerp org-capture-last-stored-marker)
             (marker-buffer org-capture-last-stored-marker))
    (let ((marker org-capture-last-stored-marker))
      (with-current-buffer (marker-buffer marker)
        (org-with-wide-buffer
         (save-excursion
           (let ((file (buffer-file-name)))
             (goto-char marker)
             ;; File-level roam nodes keep their ID in a property drawer
             ;; at the top of the file; heading-level roam nodes have it
             ;; on the heading where the marker points.  Try both.
             (let ((id (or (org-entry-get (point) "ID")
                           (save-excursion
                             (goto-char (point-min))
                             (org-entry-get (point) "ID")))))
               (when (and file id) (cons id file))))))))))

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
    (let ((captured (mcp-server-emacs-tools-org-roam-capture--last-captured)))
      (when captured
        (let ((id (car captured)) (file (cdr captured)))
          (mcp-server-emacs-tools-org-common--validate-path file)
          ;; Apply post-capture edits BEFORE syncing the DB so aliases,
          ;; refs, and filetags end up indexed.
          (when (or tags aliases refs)
            (with-current-buffer (find-file-noselect file)
              (save-excursion
                (goto-char (point-min))
                (when aliases
                  (org-set-property "ROAM_ALIASES"
                                    (mapconcat #'identity (append aliases nil) " ")))
                (when refs
                  (org-set-property "ROAM_REFS"
                                    (mapconcat #'identity (append refs nil) " ")))
                (when tags
                  ;; Direct-mode roam nodes are file-level (no top heading),
                  ;; so store tags as `#+filetags:' rather than calling
                  ;; `org-set-tags' off-heading.
                  (mcp-server-emacs-tools-org-roam-capture--set-filetags
                   (append tags nil)))
                (when mcp-server-emacs-tools-org-auto-save (save-buffer)))))
          (org-roam-db-sync)
          `((id . ,id) (file . ,file) (title . ,title)))))))

(defun mcp-server-emacs-tools-org-roam-capture--set-filetags (tags)
  "Set `#+filetags:' to TAGS (a list of strings) in the current buffer.
Replaces an existing `#+filetags:' line or inserts one after the
`#+title:' line (or at `point-min' if no title line is present)."
  (save-excursion
    (goto-char (point-min))
    (let ((line (concat "#+filetags: :"
                        (mapconcat #'identity tags ":")
                        ":")))
      (if (re-search-forward "^#\\+filetags:.*$" nil t)
          (replace-match line t t)
        (goto-char (point-min))
        (if (re-search-forward "^#\\+title:.*$" nil t)
            (progn (end-of-line) (insert "\n" line))
          (goto-char (point-min))
          (insert line "\n"))))))

(defun mcp-server-emacs-tools-org-roam-capture--template (args)
  "Run a roam capture with a user template."
  (let* ((key (alist-get 'template_key args))
         (title (or (alist-get 'title args) (error "`title' is required")))
         (content (alist-get 'content args))
         (org-capture-initial (or content ""))
         (node (org-roam-node-create :title title)))
    (org-roam-capture- :keys key :node node)
    (let ((captured (mcp-server-emacs-tools-org-roam-capture--last-captured)))
      (when captured
        (let ((id (car captured)) (file (cdr captured)))
          (mcp-server-emacs-tools-org-common--validate-path file)
          (org-roam-db-sync)
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
                                   (body . ((type . "string")))
                                   (tags . ((type . "array")
                                            (items . ((type . "string")))))
                                   (aliases . ((type . "array")
                                               (items . ((type . "string")))))
                                   (refs . ((type . "array")
                                            (items . ((type . "string")))))))
                    (required . ["title"]))
    :function #'mcp-server-emacs-tools-org-roam-capture--handler
    :annotations '((readOnlyHint . :false)
                   (destructiveHint . t)
                   (idempotentHint . :false)
                   (openWorldHint . :false)))))

(provide 'mcp-server-emacs-tools-org-roam-capture)

;;; mcp-server-emacs-tools-org-roam-capture.el ends here
