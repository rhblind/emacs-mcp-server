;;; mcp-server-emacs-tools-org-roam-get-node.el --- org-roam-get-node MCP tool -*- lexical-binding: t; -*-

;;; Commentary:

;; Fetch an org-roam node with its backlink graph.

;;; Code:

(require 'mcp-server-tools)
(require 'mcp-server-emacs-tools-org-common)
(require 'json)

(declare-function org-roam-node-from-id "org-roam-node" (id))
(declare-function org-roam-node-title "org-roam-node" (node))
(declare-function org-roam-node-file "org-roam-node" (node))
(declare-function org-roam-node-tags "org-roam-node" (node))
(declare-function org-roam-node-aliases "org-roam-node" (node))
(declare-function org-roam-node-refs "org-roam-node" (node))
(declare-function org-roam-backlinks-get "org-roam-node" (node))
(declare-function org-roam-backlink-source-node "org-roam-mode" (bl))
(declare-function org-roam-db-query "org-roam-db" (sql &rest args))
(declare-function org-roam-node-id "org-roam-node" (node))

(defun mcp-server-emacs-tools-org-roam-get-node--handler (args)
  "Handle org-roam-get-node tool call with ARGS."
  (condition-case err
      (let* ((id (or (alist-get 'id args) (error "`id' is required")))
             (include-body (alist-get 'include_body args t))
             (include-backlinks (alist-get 'include_backlinks args t))
             (include-forward (alist-get 'include_forward_links args nil))
             (backlink-limit (or (alist-get 'backlink_limit args) 50))
             (node (org-roam-node-from-id id))
             (_ (unless node (error "Roam node not found: %s" id)))
             (title (org-roam-node-title node))
             (file (org-roam-node-file node))
             (tags (org-roam-node-tags node))
             (aliases (org-roam-node-aliases node))
             (refs (org-roam-node-refs node))
             (body (when include-body
                     (let* ((marker (mcp-server-emacs-tools-org-common--resolve-node
                                     `((id . ,id))))
                            (alist (mcp-server-emacs-tools-org-common--node-to-alist
                                    marker :include-body t)))
                       (alist-get 'body alist))))
             (backlinks
              (when include-backlinks
                (let* ((bls (org-roam-backlinks-get node))
                       (sliced (seq-take bls backlink-limit))
                       (rows '()))
                  (dolist (bl sliced)
                    (let* ((src-node (org-roam-backlink-source-node bl))
                           (src-id (when src-node (org-roam-node-id src-node)))
                           (src-title (when src-node (org-roam-node-title src-node))))
                      (push `((source_id . ,src-id)
                              (source_title . ,src-title))
                            rows)))
                  (vconcat (nreverse rows)))))
             (forward
              (when include-forward
                (let ((rows (org-roam-db-query
                             [:select [dest] :from links :where (= source $s1)]
                             id)))
                  (vconcat
                   (mapcar (lambda (row)
                             (let ((n (org-roam-node-from-id (car row))))
                               `((target_id . ,(car row))
                                 (target_title . ,(and n (org-roam-node-title n))))))
                           rows))))))
        (json-encode
         `((node . ((id . ,id)
                    (title . ,title)
                    (file . ,file)
                    (tags . ,(vconcat tags))
                    (aliases . ,(vconcat aliases))
                    (refs . ,(vconcat refs))
                    (body . ,body)))
           (backlinks . ,(or backlinks []))
           (forward_links . ,(or forward [])))))
    (error (json-encode `((error . ,(error-message-string err)))))))

(when (require 'org-roam nil t)
  (mcp-server-register-tool
   (make-mcp-server-tool
    :name "org-roam-get-node"
    :title "Get Org-Roam Node"
    :description "Fetch a roam node with its backlinks and forward links.  Use for knowledge-base retrieval when backlink context is needed.  For generic org headings, use org-get-node."
    :input-schema '((type . "object")
                    (properties . ((id . ((type . "string")))
                                   (include_body . ((type . "boolean")))
                                   (include_backlinks . ((type . "boolean")))
                                   (include_forward_links . ((type . "boolean")))
                                   (backlink_limit . ((type . "number")))))
                    (required . ["id"]))
    :function #'mcp-server-emacs-tools-org-roam-get-node--handler
    :annotations '((readOnlyHint . t)
                   (destructiveHint . :false)
                   (idempotentHint . t)
                   (openWorldHint . :false)))))

(provide 'mcp-server-emacs-tools-org-roam-get-node)

;;; mcp-server-emacs-tools-org-roam-get-node.el ends here
