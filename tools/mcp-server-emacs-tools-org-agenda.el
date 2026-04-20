;;; mcp-server-emacs-tools-org-agenda.el --- org-agenda MCP tool -*- lexical-binding: t; -*-

;;; Commentary:

;; Return agenda view as structured data using org's native match
;; semantics, not by scraping the agenda buffer.

;;; Code:

(require 'mcp-server-tools)
(require 'mcp-server-emacs-tools-org-common)
(require 'org)
(require 'org-agenda)
(require 'json)

(defun mcp-server-emacs-tools-org-agenda--collect (match files)
  "Collect entries matching MATCH in FILES as serialized alists."
  (let ((entries '()))
    (dolist (file files)
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (org-map-entries
            (lambda ()
              (let ((marker (point-marker)))
                (when mcp-server-emacs-tools-org-auto-id
                  (mcp-server-emacs-tools-org-common--promote-to-id marker))
                (push (mcp-server-emacs-tools-org-common--node-to-alist
                       marker :include-body nil)
                      entries)))
            match 'file)))))
    (nreverse entries)))

(defun mcp-server-emacs-tools-org-agenda--summary (entries)
  "Build a summary alist from ENTRIES."
  (let ((by-state (make-hash-table :test 'equal)))
    (dolist (e entries)
      (let ((state (or (alist-get 'todo_state e) "none")))
        (puthash state (1+ (gethash state by-state 0)) by-state)))
    `((count . ,(length entries))
      (by_state . ,(let ((acc '()))
                     (maphash (lambda (k v) (push (cons k v) acc)) by-state)
                     acc)))))

(defun mcp-server-emacs-tools-org-agenda--handler (args)
  "Handle org-agenda tool call with ARGS."
  (condition-case err
      (let* ((view (or (alist-get 'view args) "day"))
             (files (or (alist-get 'files args) (org-agenda-files)))
             (files-list (append files nil))
             (_ (mapc #'mcp-server-emacs-tools-org-common--validate-path
                      files-list))
             (match (alist-get 'match args))
             (effective-match
              (pcase view
                ("todo" "/!")
                ("tags" (or match (error "view=tags requires `match'")))
                ("search" (or match (error "view=search requires `match'")))
                ("day" "SCHEDULED<=\"<+1d>\"|DEADLINE<=\"<+1d>\"")
                ("week" "SCHEDULED<=\"<+7d>\"|DEADLINE<=\"<+7d>\"")
                (_ (error "Unknown view: %s" view))))
             (entries (mcp-server-emacs-tools-org-agenda--collect
                       effective-match files-list)))
        (json-encode `((entries . ,(vconcat entries))
                       (summary . ,(mcp-server-emacs-tools-org-agenda--summary entries)))))
    (error (json-encode `((error . ,(error-message-string err)))))))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "org-agenda"
  :title "Org Agenda View"
  :description "Return agenda/TODO view as structured data.  Views: day, week, todo, tags, search.  tags/search require `match'.  When `mcp-server-emacs-tools-org-auto-id' is non-nil (default), matched headings that lack an ID are assigned one so later calls can reference them; set the defcustom to nil to disable that side effect."
  :input-schema '((type . "object")
                  (properties . ((view . ((type . "string")
                                          (enum . ("day" "week" "todo" "tags" "search"))))
                                 (match . ((type . "string")))
                                 (files . ((type . "array")
                                           (items . ((type . "string")))))))
                  (required . []))
  :function #'mcp-server-emacs-tools-org-agenda--handler
  :annotations '((readOnlyHint . :false)
                 (destructiveHint . t)
                 (idempotentHint . t)
                 (openWorldHint . :false))))

(provide 'mcp-server-emacs-tools-org-agenda)

;;; mcp-server-emacs-tools-org-agenda.el ends here
