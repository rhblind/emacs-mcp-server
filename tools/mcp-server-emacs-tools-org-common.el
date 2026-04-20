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

(defun mcp-server-emacs-tools-org-common--truthy-p (value)
  "Return non-nil when VALUE should be treated as true after JSON parsing.
JSON `false' and `null' parsed by `json-parse-string' become the
non-nil symbols `:false' and `:null'.  Treat both, plus nil, as false."
  (and value
       (not (eq value :false))
       (not (eq value :null))))

(defun mcp-server-emacs-tools-org-common--bool-arg (args key default)
  "Return the boolean value of KEY in ARGS, or DEFAULT when absent.
Normalizes JSON-parsed `:false' / `:null' to nil."
  (let ((v (alist-get key args 'mcp-absent)))
    (if (eq v 'mcp-absent)
        default
      (mcp-server-emacs-tools-org-common--truthy-p v))))

(defun mcp-server-emacs-tools-org-common--non-negative-integer (value default)
  "Coerce VALUE to a non-negative integer, falling back to DEFAULT.
Accepts integers and floats (truncated); nil / non-numeric values
return DEFAULT.  Negative results are clamped to 0.  DEFAULT must
already be an integer or nil."
  (cond
   ((integerp value) (max 0 value))
   ((floatp value) (max 0 (truncate value)))
   (t default)))

(defun mcp-server-emacs-tools-org-common--clearable-arg (args key)
  "Return the state of a clearable field KEY in ARGS.
Returns one of:
  `unset' - key was not supplied
  `clear' - key was supplied as nil or JSON null (`:null')
  STRING  - supplied value"
  (let ((v (alist-get key args 'mcp-absent)))
    (cond
     ((eq v 'mcp-absent) 'unset)
     ((or (null v) (eq v :null)) 'clear)
     (t v))))

(defun mcp-server-emacs-tools-org-common--resolve-node (args)
  "Resolve ARGS to a marker pointing at an org heading.
ARGS is an alist that must include either:
  (id . STRING) - org-id; looked up via `org-id-find'
  (file . STRING) + (outline_path . VECTOR) - looked up via `org-find-olp'
The resolved file is always validated against
`mcp-server-emacs-tools-org-allowed-roots'; IDs that resolve to files
outside the allowed roots are rejected.  Signals an error if the node
cannot be located or the file is not allowed."
  (let ((id (alist-get 'id args))
        (file (alist-get 'file args))
        (olp (alist-get 'outline_path args)))
    (cond
     (id
      (let ((location (org-id-find id 'marker)))
        (unless (markerp location)
          (error "Org node not found for id: %s" id))
        (when-let ((resolved-file (buffer-file-name (marker-buffer location))))
          (mcp-server-emacs-tools-org-common--validate-path resolved-file))
        location))
     ((and file olp)
      (mcp-server-emacs-tools-org-common--validate-path file)
      (let* ((buf (find-file-noselect file))
             (path-list (append olp nil)))
        (condition-case err
            (with-current-buffer buf
              (org-find-olp (cons file path-list)))
          (error
           (error "Outline path not found in %s: %S" file path-list)))))
     (file
      (mcp-server-emacs-tools-org-common--validate-path file)
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

(defun mcp-server-emacs-tools-org-common--truncate-to-bytes (str limit)
  "Return a prefix of STR whose byte length is at most LIMIT.
Truncation happens at a character boundary, never mid-codepoint.  STR
is returned unchanged when its byte length is already within LIMIT."
  (if (<= (string-bytes str) limit)
      str
    (let ((end (min (length str) limit)))
      (while (and (> end 0)
                  (> (string-bytes (substring str 0 end)) limit))
        (setq end (1- end)))
      (substring str 0 end))))

(defun mcp-server-emacs-tools-org-common--extract-body-at-point ()
  "Extract the body text relevant to point.
On a heading, returns text between the heading's `:contents-begin' and
`:contents-end' as reported by `org-element'.  Note that `:contents-end'
ends before the first child heading, so the result includes the
heading's own body (property drawer, planning lines, paragraphs) but
NOT child subtrees.  Off a heading (file-level node), returns
pre-heading content: everything from `point-min' to the first heading,
or the whole file if there are no headings.  Returns a cons cell
\(BODY . TRUNCATED) where BODY is the possibly byte-truncated string and
TRUNCATED is t when the raw value exceeds
`mcp-server-emacs-tools-org-max-body-bytes'."
  (let* ((limit mcp-server-emacs-tools-org-max-body-bytes)
         (raw
          (cond
           ((org-at-heading-p)
            (let* ((element (org-element-at-point))
                   (begin (and element (org-element-property :contents-begin element)))
                   (end (and element (org-element-property :contents-end element))))
              (if (and begin end)
                  (buffer-substring-no-properties begin end)
                "")))
           (t
            (let ((begin (point-min))
                  (end (save-excursion
                         (goto-char (point-min))
                         (if (re-search-forward org-heading-regexp nil t)
                             (match-beginning 0)
                           (point-max)))))
              (buffer-substring-no-properties begin end)))))
         (truncated (> (string-bytes raw) limit)))
    (cons (if truncated
              (mcp-server-emacs-tools-org-common--truncate-to-bytes raw limit)
            raw)
          truncated)))

(cl-defun mcp-server-emacs-tools-org-common--node-to-alist
    (marker &key (include-body t))
  "Serialize node at MARKER to an alist.
When INCLUDE-BODY is non-nil, include the node body, possibly
byte-truncated to `mcp-server-emacs-tools-org-max-body-bytes'.
Supports both heading-based markers (extracts the heading's own body
up to the first child heading) and file-level markers (extracts
pre-heading content)."
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
         (let ((bt (mcp-server-emacs-tools-org-common--extract-body-at-point)))
           (setq body (car bt))
           (setq truncated (cdr bt))))
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

(defun mcp-server-emacs-tools-org-common--effective-roots ()
  "Return the effective list of allowed directory roots.
Uses `mcp-server-emacs-tools-org-allowed-roots' if set, otherwise
falls back to directories containing `org-directory' and
`org-agenda-files'."
  (or mcp-server-emacs-tools-org-allowed-roots
      (let ((roots '()))
        (when (bound-and-true-p org-directory)
          (push (file-name-as-directory (expand-file-name org-directory))
                roots))
        (dolist (f (and (bound-and-true-p org-agenda-files)
                        (if (functionp 'org-agenda-files)
                            (org-agenda-files)
                          org-agenda-files)))
          (when (stringp f)
            (push (file-name-as-directory
                   (expand-file-name (file-name-directory f)))
                  roots)))
        (delete-dups roots))))

(defun mcp-server-emacs-tools-org-common--validate-path (path)
  "Validate PATH is inside an allowed root.
Compares true names on both sides so symlinked roots (e.g. macOS
`/tmp' -> `/private/tmp') match correctly.  Returns PATH (expanded)
on success; signals an error on failure."
  (let* ((abs (expand-file-name path))
         (abs-true (file-truename abs))
         (roots (mcp-server-emacs-tools-org-common--effective-roots)))
    (unless roots
      (error "No allowed roots configured; set `mcp-server-emacs-tools-org-allowed-roots' or `org-directory'"))
    (unless (cl-some (lambda (root)
                       (let ((root-true (file-name-as-directory
                                         (file-truename
                                          (expand-file-name root)))))
                         (or (string-prefix-p root-true abs-true)
                             (string-prefix-p root-true
                                              (file-name-as-directory abs-true)))))
                     roots)
      (error "Path %s is outside allowed roots: %S" abs roots))
    abs))

(defmacro mcp-server-emacs-tools-org-common--with-node (marker &rest body)
  "Execute BODY at MARKER's buffer and point.
Wraps in `save-excursion'.  When `mcp-server-emacs-tools-org-auto-save'
is non-nil and the buffer is visiting a file, save the buffer after BODY."
  (declare (indent 1))
  `(let ((marker ,marker))
     (with-current-buffer (marker-buffer marker)
       (save-excursion
         (goto-char marker)
         (prog1 (progn ,@body)
           (when (and mcp-server-emacs-tools-org-auto-save
                      (buffer-file-name)
                      (buffer-modified-p))
             (save-buffer)))))))

(defun mcp-server-emacs-tools-org-common--promote-to-id (marker)
  "Ensure the node at MARKER has an ID; return it.
When `mcp-server-emacs-tools-org-auto-id' is nil, returns the existing
ID or nil without creating one."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (if mcp-server-emacs-tools-org-auto-id
          (org-id-get-create)
        (org-entry-get nil "ID")))))

(defconst mcp-server-emacs-tools-org-common--roam-hint
  " If org-roam is installed, consider `org-roam-search' for knowledge-base queries."
  "Appended to org-search-style descriptions when org-roam is present.")

(defun mcp-server-emacs-tools-org-common--augment-description (base hint-kind)
  "Append a roam hint to BASE description when appropriate.
HINT-KIND is currently only `roam-hint'.  If org-roam is not loaded,
BASE is returned unchanged."
  (cond
   ((and (eq hint-kind 'roam-hint) (featurep 'org-roam))
    (concat base mcp-server-emacs-tools-org-common--roam-hint))
   (t base)))

(provide 'mcp-server-emacs-tools-org-common)

;;; mcp-server-emacs-tools-org-common.el ends here
