;;; mcp-server-emacs-tools-org-list-tags.el --- org-list-tags MCP tool -*- lexical-binding: t; -*-

;;; Commentary:

;; Return the corpus of tags with usage counts so LLM clients can reuse
;; existing tags rather than creating near-duplicates.

;;; Code:

(require 'mcp-server-tools)
(require 'mcp-server-emacs-tools-org-common)
(require 'org)
(require 'json)
(require 'seq)

(defun mcp-server-emacs-tools-org-list-tags--configured-tags ()
  "Return configured tag names from `org-tag-alist' and `org-tag-persistent-alist'.
In-buffer `#+TAGS:' directives are not included here; they show up
through the used-tags path when the buffer they appear in is scanned."
  (let ((seen (make-hash-table :test 'equal)))
    (dolist (entry (append (and (boundp 'org-tag-alist) org-tag-alist)
                           (and (boundp 'org-tag-persistent-alist)
                                org-tag-persistent-alist)))
      (when (and (consp entry) (stringp (car entry)))
        (puthash (car entry) t seen)))
    (hash-table-keys seen)))

(defun mcp-server-emacs-tools-org-list-tags--scope-files (scope files directory)
  "Resolve SCOPE to a list of files, validating each against allowed roots.
Mirrors `mcp-server-emacs-tools-org-search--scope-files'."
  (let ((resolved
         (pcase scope
           ("agenda" (org-agenda-files))
           ("file"
            (unless files (error "scope=file requires `files'"))
            (append files nil))
           ("directory"
            (unless directory (error "scope=directory requires `directory'"))
            (mcp-server-emacs-tools-org-common--validate-path directory)
            (directory-files-recursively directory "\\.org$"))
           ("roam" nil)
           (_ (error "Unknown scope: %s" scope)))))
    (mapc #'mcp-server-emacs-tools-org-common--validate-path resolved)
    resolved))

(defun mcp-server-emacs-tools-org-list-tags--used-tags-in-files (files)
  "Return a hash table of {tag -> count} from FILES."
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (file files)
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (org-map-entries
            (lambda ()
              (dolist (tag (org-get-tags nil t))
                (puthash tag (1+ (gethash tag counts 0)) counts)))
            t 'file)))))
    counts))

(defun mcp-server-emacs-tools-org-list-tags--roam-tags ()
  "Return a hash table of {tag -> count} from the org-roam DB.
Counting is done client-side because emacsql aggregate syntax varies
across org-roam versions."
  (let ((counts (make-hash-table :test 'equal)))
    (when (and (featurep 'org-roam)
               (fboundp 'org-roam-db-query))
      (dolist (row (org-roam-db-query [:select tag :from tags]))
        (let ((tag (car row)))
          (puthash tag (1+ (gethash tag counts 0)) counts))))
    counts))

(defun mcp-server-emacs-tools-org-list-tags--handler (args)
  "Handle org-list-tags tool call with ARGS."
  (condition-case err
      (let* ((scope (or (alist-get 'scope args) "agenda"))
             (files (alist-get 'files args))
             (directory (alist-get 'directory args))
             (include-configured (mcp-server-emacs-tools-org-common--bool-arg
                                  args 'include_configured t))
             (limit (alist-get 'limit args))
             (counts
              (pcase scope
                ("roam"
                 (unless (featurep 'org-roam)
                   (error "org-roam not installed"))
                 (mcp-server-emacs-tools-org-list-tags--roam-tags))
                (_ (mcp-server-emacs-tools-org-list-tags--used-tags-in-files
                    (mcp-server-emacs-tools-org-list-tags--scope-files
                     scope files directory)))))
             (configured (when (and include-configured (not (equal scope "roam")))
                           (mcp-server-emacs-tools-org-list-tags--configured-tags)))
             (all-tags (make-hash-table :test 'equal))
             (result '()))
        (maphash
         (lambda (name count)
           (puthash name `((count . ,count) (used . t) (configured . nil))
                    all-tags))
         counts)
        (dolist (name configured)
          (let ((existing (gethash name all-tags)))
            (if existing
                (puthash name
                         `((count . ,(alist-get 'count existing))
                           (used . ,(alist-get 'used existing))
                           (configured . t))
                         all-tags)
              (puthash name
                       '((count . 0) (used . nil) (configured . t))
                       all-tags))))
        (maphash
         (lambda (name info)
           (let ((sources '()))
             (when (alist-get 'configured info) (push "configured" sources))
             (when (alist-get 'used info) (push "used" sources))
             (push `((name . ,name)
                     (count . ,(alist-get 'count info))
                     (source . ,(vconcat (nreverse sources))))
                   result)))
         all-tags)
        (setq result (sort result
                           (lambda (a b) (> (alist-get 'count a)
                                            (alist-get 'count b)))))
        (when limit
          (setq result (seq-take result limit)))
        (json-encode `((tags . ,(vconcat result))
                       (total_unique . ,(hash-table-count all-tags)))))
    (error (json-encode `((error . ,(error-message-string err)))))))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "org-list-tags"
  :title "List Org Tags"
  :description "Return the tag corpus with usage counts so you can prefer existing tags.  Sort order is count-descending.  Before adding new tags via org-update-node or org-capture, check this list to avoid near-duplicates (pluralization, casing, synonyms)."
  :input-schema '((type . "object")
                  (properties . ((scope . ((type . "string")
                                           (enum . ("agenda" "file" "directory" "roam"))))
                                 (files . ((type . "array")
                                           (items . ((type . "string")))))
                                 (directory . ((type . "string")))
                                 (include_configured . ((type . "boolean")))
                                 (limit . ((type . "number")))))
                  (required . []))
  :function #'mcp-server-emacs-tools-org-list-tags--handler
  :annotations '((readOnlyHint . t)
                 (destructiveHint . :false)
                 (idempotentHint . t)
                 (openWorldHint . :false))))

(provide 'mcp-server-emacs-tools-org-list-tags)

;;; mcp-server-emacs-tools-org-list-tags.el ends here
