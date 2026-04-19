;;; mcp-server-emacs-tools-org-search.el --- org-search MCP tool -*- lexical-binding: t; -*-

;;; Commentary:

;; Query headings across org files using org's native match syntax.

;;; Code:

(require 'mcp-server-tools)
(require 'mcp-server-emacs-tools-org-common)
(require 'org)
(require 'json)

(defun mcp-server-emacs-tools-org-search--scope-files (scope files directory)
  "Resolve SCOPE to a list of files, validating each against allowed roots.
SCOPE is a string; FILES and DIRECTORY are optional supporting args."
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
           (_ (error "Unknown scope: %s" scope)))))
    (mapc #'mcp-server-emacs-tools-org-common--validate-path resolved)
    resolved))

(defun mcp-server-emacs-tools-org-search--handler (args)
  "Handle org-search tool call with ARGS."
  (condition-case err
      (let* ((match (or (alist-get 'match args)
                        (error "`match' is required")))
             (scope (or (alist-get 'scope args) "agenda"))
             (files (alist-get 'files args))
             (directory (alist-get 'directory args))
             (limit (mcp-server-emacs-tools-org-common--non-negative-integer
                     (alist-get 'limit args) 50))
             (max-limit 500)
             (effective-limit (min limit max-limit))
             (resolved (mcp-server-emacs-tools-org-search--scope-files
                        scope files directory))
             (results '())
             (count 0)
             (truncated nil))
        (catch 'done
          (dolist (file resolved)
            (when (file-exists-p file)
              (with-current-buffer (find-file-noselect file)
                (org-with-wide-buffer
                 (org-map-entries
                  (lambda ()
                    (when (>= count effective-limit)
                      (setq truncated t)
                      (throw 'done nil))
                    (let ((marker (point-marker)))
                      (when mcp-server-emacs-tools-org-auto-id
                        (mcp-server-emacs-tools-org-common--promote-to-id marker))
                      (push (mcp-server-emacs-tools-org-common--node-to-alist
                             marker :include-body nil)
                            results)
                      (setq count (1+ count))))
                  match 'file))))))
        (json-encode `((results . ,(vconcat (nreverse results)))
                       (truncated . ,(if truncated t :false)))))
    (error (json-encode `((error . ,(error-message-string err)))))))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "org-search"
  :title "Search Org Headings"
  :description
  (mcp-server-emacs-tools-org-common--augment-description
   "Search TODOs, agenda items, and headings across org files using org's native match syntax (e.g. \"+work-home/!TODO\").  Returns summaries without bodies; use org-get-node to fetch body content."
   'roam-hint)
  :input-schema '((type . "object")
                  (properties . ((match . ((type . "string")
                                           (description . "Org match expression (required)")))
                                 (scope . ((type . "string")
                                           (enum . ("agenda" "file" "directory"))))
                                 (files . ((type . "array")
                                           (items . ((type . "string")))))
                                 (directory . ((type . "string")))
                                 (limit . ((type . "integer")))))
                  (required . ["match"]))
  :function #'mcp-server-emacs-tools-org-search--handler
  :annotations '((readOnlyHint . t)
                 (destructiveHint . :false)
                 (idempotentHint . t)
                 (openWorldHint . :false))))

(provide 'mcp-server-emacs-tools-org-search)

;;; mcp-server-emacs-tools-org-search.el ends here
