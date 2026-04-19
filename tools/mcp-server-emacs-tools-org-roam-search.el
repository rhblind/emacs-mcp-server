;;; mcp-server-emacs-tools-org-roam-search.el --- org-roam-search MCP tool -*- lexical-binding: t; -*-

;;; Commentary:

;; Search org-roam nodes via org-roam-db-query.  Registers only when
;; org-roam is installed and loadable.

;;; Code:

(require 'mcp-server-tools)
(require 'mcp-server-emacs-tools-org-common)
(require 'cl-lib)
(require 'json)

(declare-function org-roam-db-query "org-roam-db" (sql &rest args))
(declare-function org-roam-node-from-id "org-roam-node" (id))
(declare-function org-roam-node-title "org-roam-node" (node))
(declare-function org-roam-node-file "org-roam-node" (node))
(declare-function org-roam-node-tags "org-roam-node" (node))

(defun mcp-server-emacs-tools-org-roam-search--build-index (sql)
  "Run SQL (a 2-column query returning [node-id VALUE]) and return {id -> (values...)}."
  (let ((idx (make-hash-table :test 'equal)))
    (dolist (row (org-roam-db-query sql))
      (let ((id (nth 0 row)) (v (nth 1 row)))
        (puthash id (cons v (gethash id idx)) idx)))
    idx))

(defun mcp-server-emacs-tools-org-roam-search--handler (args)
  "Handle org-roam-search tool call with ARGS.
Loads nodes and related (tags, aliases, refs) in four queries total,
then matches client-side.  Previous implementation issued three
subqueries per node, which scaled linearly with DB size."
  (condition-case err
      (let* ((query (alist-get 'query args))
             (tags (alist-get 'tags args))
             (refs (alist-get 'refs args))
             (limit (mcp-server-emacs-tools-org-common--non-negative-integer
                     (alist-get 'limit args) 25))
             (max-limit 200)
             (effective-limit (min limit max-limit))
             (rows (org-roam-db-query
                    [:select [id title file level] :from nodes]))
             (tag-idx (mcp-server-emacs-tools-org-roam-search--build-index
                       [:select [node-id tag] :from tags]))
             (alias-idx (mcp-server-emacs-tools-org-roam-search--build-index
                         [:select [node-id alias] :from aliases]))
             (ref-idx (mcp-server-emacs-tools-org-roam-search--build-index
                       [:select [node-id ref] :from refs]))
             (results '())
             (count 0)
             (truncated nil))
        (catch 'mcp-roam-search-done
          (dolist (row rows)
            (let* ((id (nth 0 row))
                   (title (nth 1 row))
                   (file (nth 2 row))
                   (level (nth 3 row))
                   ;; Path safety: skip nodes whose file is outside
                   ;; `mcp-server-emacs-tools-org-allowed-roots'.  This
                   ;; prevents enumerating paths/metadata for disallowed
                   ;; files via the roam DB.
                   (file-ok (condition-case nil
                                (progn
                                  (mcp-server-emacs-tools-org-common--validate-path file)
                                  t)
                              (error nil)))
                   (node-tags (gethash id tag-idx))
                   (node-aliases (gethash id alias-idx))
                   (node-refs (gethash id ref-idx))
                   (matches
                    (and
                     file-ok
                     (or (null query)
                         (string-match-p (regexp-quote query) (or title ""))
                         (cl-some (lambda (a) (string-match-p (regexp-quote query) a))
                                  node-aliases))
                     (or (null tags)
                         (cl-every (lambda (t-wanted) (member t-wanted node-tags))
                                   (append tags nil)))
                     (or (null refs)
                         (cl-every (lambda (r) (member r node-refs))
                                   (append refs nil))))))
              (when matches
                (if (>= count effective-limit)
                    (progn (setq truncated t)
                           (throw 'mcp-roam-search-done nil))
                  (push `((id . ,id)
                          (title . ,title)
                          (file . ,file)
                          (level . ,level)
                          (tags . ,(vconcat node-tags))
                          (aliases . ,(vconcat node-aliases))
                          (refs . ,(vconcat node-refs)))
                        results)
                  (setq count (1+ count)))))))
        (json-encode `((results . ,(vconcat (nreverse results)))
                       (truncated . ,(if truncated t :false)))))
    (error (json-encode `((error . ,(error-message-string err)))))))

(when (require 'org-roam nil t)
  (mcp-server-register-tool
   (make-mcp-server-tool
    :name "org-roam-search"
    :title "Search Org-Roam Nodes"
    :description "Find org-roam notes by title, alias, tag, or ref.  Returns node IDs usable with org-roam-get-node or org-get-node.  For scheduled tasks or agenda entries, prefer org-search instead."
    :input-schema '((type . "object")
                    (properties . ((query . ((type . "string")))
                                   (tags . ((type . "array")
                                            (items . ((type . "string")))))
                                   (refs . ((type . "array")
                                            (items . ((type . "string")))))
                                   (limit . ((type . "integer")))))
                    (required . []))
    :function #'mcp-server-emacs-tools-org-roam-search--handler
    :annotations '((readOnlyHint . t)
                   (destructiveHint . :false)
                   (idempotentHint . t)
                   (openWorldHint . :false)))))

(provide 'mcp-server-emacs-tools-org-roam-search)

;;; mcp-server-emacs-tools-org-roam-search.el ends here
