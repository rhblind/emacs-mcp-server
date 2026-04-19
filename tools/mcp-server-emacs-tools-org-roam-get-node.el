;;; mcp-server-emacs-tools-org-roam-get-node.el --- org-roam-get-node MCP tool -*- lexical-binding: t; -*-

;;; Commentary:

;; Fetch an org-roam node with its backlink graph.

;;; Code:

(require 'mcp-server-tools)
(require 'mcp-server-emacs-tools-org-common)
(require 'json)
(require 'seq)

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
             (include-body (mcp-server-emacs-tools-org-common--bool-arg
                            args 'include_body t))
             (include-backlinks (mcp-server-emacs-tools-org-common--bool-arg
                                 args 'include_backlinks t))
             (include-forward (mcp-server-emacs-tools-org-common--bool-arg
                               args 'include_forward_links nil))
             (backlink-limit (mcp-server-emacs-tools-org-common--non-negative-integer
                              (alist-get 'backlink_limit args) 50))
             (node (org-roam-node-from-id id))
             (_ (unless node (error "Roam node not found: %s" id)))
             (title (org-roam-node-title node))
             (file (org-roam-node-file node))
             ;; Path safety: validate up front regardless of `include_body',
             ;; so clients cannot enumerate file paths for roam nodes outside
             ;; `mcp-server-emacs-tools-org-allowed-roots'.
             (_ (when file (mcp-server-emacs-tools-org-common--validate-path file)))
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
                ;; Restrict to id-type links and inner-join against nodes so
                ;; every target is guaranteed to be a real roam node.  Keeps
                ;; us clear of file:/http:/etc link targets stored in the
                ;; `links' table.
                (let ((rows (org-roam-db-query
                             [:select [links:dest nodes:title]
                              :from links
                              :inner-join nodes
                              :on (= links:dest nodes:id)
                              :where (and (= links:source $s1)
                                          (= links:type "id"))]
                             id)))
                  (vconcat
                   (mapcar (lambda (row)
                             `((target_id . ,(nth 0 row))
                               (target_title . ,(nth 1 row))))
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
                                   (backlink_limit . ((type . "integer")))))
                    (required . ["id"]))
    :function #'mcp-server-emacs-tools-org-roam-get-node--handler
    :annotations '((readOnlyHint . t)
                   (destructiveHint . :false)
                   (idempotentHint . t)
                   (openWorldHint . :false)))))

(provide 'mcp-server-emacs-tools-org-roam-get-node)

;;; mcp-server-emacs-tools-org-roam-get-node.el ends here
