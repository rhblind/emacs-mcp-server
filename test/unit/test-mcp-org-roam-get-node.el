;;; test-mcp-org-roam-get-node.el --- Tests for org-roam-get-node -*- lexical-binding: t; -*-
(require 'ert)
(require 'test-helpers)
(require 'json)

(let* ((this-file (or load-file-name buffer-file-name))
       (tools-dir (and this-file
                       (expand-file-name "../../tools"
                                         (file-name-directory this-file)))))
  (when tools-dir (add-to-list 'load-path tools-dir)))

(require 'mcp-server-emacs-tools-org-roam-get-node)

(defconst mcp-test-roam-available
  (and (>= emacs-major-version 29)
       (require 'org-roam nil t)))

(ert-deftest mcp-test-org-roam-get-node-registration-matches-availability ()
  "Tool registration matches org-roam availability."
  (if (featurep 'org-roam)
      (should (mcp-server-tools-exists-p "org-roam-get-node"))
    (should-not (mcp-server-tools-exists-p "org-roam-get-node"))))

(ert-deftest mcp-test-org-roam-get-node-returns-node ()
  "get-node returns title, file, tags, aliases, refs for known id."
  (skip-unless mcp-test-roam-available)
  (mcp-test-with-roam-fixture dir
    (let* ((json (mcp-server-emacs-tools-org-roam-get-node--handler
                  '((id . "roam-concept-b-0001")
                    (include_backlinks . :false))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json)))
           (node (alist-get 'node result)))
      (should (equal (alist-get 'title node) "Concept B"))
      (should (stringp (alist-get 'file node)))
      (should (member "example" (append (alist-get 'tags node) nil))))))

(ert-deftest mcp-test-org-roam-get-node-returns-backlinks ()
  "get-node includes backlinks when requested."
  (skip-unless mcp-test-roam-available)
  (mcp-test-with-roam-fixture dir
    (let* ((json (mcp-server-emacs-tools-org-roam-get-node--handler
                  '((id . "roam-concept-a-0001")
                    (include_backlinks . t))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json)))
           (bl (alist-get 'backlinks result))
           (src-ids (mapcar (lambda (b) (alist-get 'source_id b)) (append bl nil))))
      (should (member "roam-project-notes-0001" src-ids)))))

(ert-deftest mcp-test-org-roam-get-node-missing ()
  "get-node returns error JSON for unknown id."
  (skip-unless mcp-test-roam-available)
  (mcp-test-with-roam-fixture dir
    (let* ((json (mcp-server-emacs-tools-org-roam-get-node--handler
                  '((id . "does-not-exist"))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json))))
      (should (alist-get 'error result)))))

(provide 'test-mcp-org-roam-get-node)
;;; test-mcp-org-roam-get-node.el ends here
