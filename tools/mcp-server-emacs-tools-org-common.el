;;; mcp-server-emacs-tools-org-common.el --- Shared helpers for org tools -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Shared helpers for the org-* and org-roam-* MCP tools.  Provides node
;; resolution, serialization, path validation, and a with-node macro.
;; All operations go through named org APIs; no text scraping.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'org-element)

(defgroup mcp-server-emacs-tools-org nil
  "Configuration for org-mode MCP tools."
  :group 'mcp-server-emacs-tools
  :prefix "mcp-server-emacs-tools-org-")

(defcustom mcp-server-emacs-tools-org-auto-save t
  "When non-nil, write tools save affected buffers after modification."
  :type 'boolean
  :group 'mcp-server-emacs-tools-org)

(defcustom mcp-server-emacs-tools-org-auto-id t
  "When non-nil, read tools assign IDs to nodes they return."
  :type 'boolean
  :group 'mcp-server-emacs-tools-org)

(defcustom mcp-server-emacs-tools-org-allowed-roots nil
  "Directories in which org tools may read or write files.
If nil, falls back to directories containing `org-directory' and
`org-agenda-files'.  Paths outside these roots are rejected."
  :type '(repeat directory)
  :group 'mcp-server-emacs-tools-org)

(defcustom mcp-server-emacs-tools-org-max-body-bytes 100000
  "Maximum body bytes returned by org-get-node and org-roam-get-node.
Larger bodies are truncated and the response carries a `truncated' flag."
  :type 'integer
  :group 'mcp-server-emacs-tools-org)

(defun mcp-server-emacs-tools-org-common--resolve-node (args)
  "Resolve ARGS to a marker pointing at an org heading.
ARGS is an alist that must include either:
  (id . STRING) - org-id; looked up via `org-id-find'
  (file . STRING) + (outline_path . VECTOR) - looked up via `org-find-olp'
Signals an error if the node cannot be located."
  (let ((id (alist-get 'id args))
        (file (alist-get 'file args))
        (olp (alist-get 'outline_path args)))
    (cond
     (id
      (let ((location (org-id-find id 'marker)))
        (unless (markerp location)
          (error "Org node not found for id: %s" id))
        location))
     ((and file olp)
      (let* ((buf (find-file-noselect file))
             (path-list (append olp nil)))
        (condition-case err
            (with-current-buffer buf
              (org-find-olp (cons file path-list)))
          (error
           (error "Outline path not found in %s: %S" file path-list)))))
     (file
      (let ((buf (find-file-noselect file)))
        (with-current-buffer buf
          (save-excursion
            (goto-char (point-min))
            (point-marker)))))
     (t
      (error "resolve-node requires `id' or `file' (+ optional `outline_path')")))))

(defun mcp-server-emacs-tools-org-common--compute-outline-path ()
  "Return the outline path at point as a list of strings."
  (org-with-wide-buffer
   (let ((path '()))
     (save-excursion
       (while (org-up-heading-safe)
         (push (org-get-heading t t t t) path)))
     (when (org-at-heading-p)
       (setq path (append path (list (org-get-heading t t t t)))))
     path)))

(cl-defun mcp-server-emacs-tools-org-common--node-to-alist
    (marker &key (include-body t))
  "Serialize node at MARKER to an alist.
When INCLUDE-BODY is non-nil, include the node body, possibly truncated
to `mcp-server-emacs-tools-org-max-body-bytes'."
  (with-current-buffer (marker-buffer marker)
    (org-with-wide-buffer
     (goto-char marker)
     (let* ((id (org-entry-get nil "ID"))
            (title (when (org-at-heading-p) (org-get-heading t t t t)))
            (level (when (org-at-heading-p) (org-outline-level)))
            (olp (mcp-server-emacs-tools-org-common--compute-outline-path))
            (tags (when (org-at-heading-p) (org-get-tags nil t)))
            (todo-state (when (org-at-heading-p) (org-get-todo-state)))
            (priority (when (and (org-at-heading-p)
                                 (looking-at org-heading-regexp))
                        (let ((p (match-string 3)))
                          (when p (substring p 2 3)))))
            (scheduled (org-entry-get nil "SCHEDULED"))
            (deadline (org-entry-get nil "DEADLINE"))
            (properties (org-entry-properties nil 'standard))
            (file (buffer-file-name))
            (body nil)
            (truncated nil))
       (when include-body
         (let ((element (and (org-at-heading-p) (org-element-at-point))))
           (when element
             (let* ((begin (org-element-property :contents-begin element))
                    (end (org-element-property :contents-end element))
                    (raw (if (and begin end) (buffer-substring-no-properties begin end) "")))
               (setq body (if (> (length raw) mcp-server-emacs-tools-org-max-body-bytes)
                              (progn (setq truncated t)
                                     (substring raw 0 mcp-server-emacs-tools-org-max-body-bytes))
                            raw))))))
       (let ((result `((id . ,id)
                       (title . ,title)
                       (file . ,file)
                       (outline_path . ,(vconcat olp))
                       (level . ,level)
                       (todo_state . ,todo-state)
                       (tags . ,(vconcat tags))
                       (priority . ,priority)
                       (scheduled . ,scheduled)
                       (deadline . ,deadline)
                       (properties . ,properties))))
         (when include-body
           (setq result (append result `((body . ,body)))))
         (when truncated
           (setq result (append result '((truncated . t)))))
         result)))))

(provide 'mcp-server-emacs-tools-org-common)

;;; mcp-server-emacs-tools-org-common.el ends here
