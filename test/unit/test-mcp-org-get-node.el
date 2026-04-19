;;; test-mcp-org-get-node.el --- Tests for org-get-node -*- lexical-binding: t; -*-
(require 'ert)
(require 'test-helpers)
(require 'json)

(let* ((this-file (or load-file-name buffer-file-name))
       (tools-dir (and this-file
                       (expand-file-name "../../tools"
                                         (file-name-directory this-file)))))
  (when tools-dir (add-to-list 'load-path tools-dir)))

(require 'mcp-server-emacs-tools-org-get-node)

(ert-deftest mcp-test-org-get-node-by-id ()
  "get-node returns expected data for a known id."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (let* ((json (mcp-server-emacs-tools-org-get-node--handler
                  '((id . "alpha-design-0001"))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json))))
      (should (equal (alist-get 'id result) "alpha-design-0001"))
      (should (equal (alist-get 'title result) "Design"))
      (should (string-match-p "Design notes" (alist-get 'body result))))))

(ert-deftest mcp-test-org-get-node-by-olp ()
  "get-node returns data when referenced by outline path."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (let* ((json (mcp-server-emacs-tools-org-get-node--handler
                  `((file . ,path)
                    (outline_path . ["Project Alpha" "Implementation"]))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json))))
      (should (equal (alist-get 'title result) "Implementation")))))

(ert-deftest mcp-test-org-get-node-missing ()
  "get-node returns error JSON for unknown id."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (let* ((json (mcp-server-emacs-tools-org-get-node--handler
                  '((id . "nonexistent-id"))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json))))
      (should (alist-get 'error result)))))

(ert-deftest mcp-test-org-get-node-registered ()
  "Tool is registered."
  (should (mcp-server-tools-exists-p "org-get-node")))

(ert-deftest mcp-test-org-get-node-include-body-json-false-omits-body ()
  "include_body set to JSON false (:false) omits body.
Regression test: real MCP calls arrive with `:false', not elisp nil."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (let* ((json (mcp-server-emacs-tools-org-get-node--handler
                  '((id . "alpha-design-0001")
                    (include_body . :false))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json))))
      (should-not (alist-get 'body result)))))

(ert-deftest mcp-test-org-get-node-include-children-json-false-omits-children ()
  "include_children set to JSON false (:false) omits children."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (let* ((json (mcp-server-emacs-tools-org-get-node--handler
                  '((id . "alpha-root-0001")
                    (include_children . :false))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json))))
      (should-not (alist-get 'children result)))))

(ert-deftest mcp-test-org-get-node-include-children-t-returns-children ()
  "include_children t returns child headings (positive control)."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (let* ((json (mcp-server-emacs-tools-org-get-node--handler
                  '((id . "alpha-root-0001")
                    (include_children . t))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json)))
           (children (alist-get 'children result)))
      (should (vectorp children))
      (should (> (length children) 0)))))

(ert-deftest mcp-test-org-get-node-file-level-body ()
  "File-only call returns pre-heading content as `body'.
Regression test for the case where a `file' (without `outline_path')
was resolved but `--node-to-alist' returned no body because point
wasn't on a heading."
  (mcp-test-with-org-fixture "sample-notes.org" path
    ;; sample-notes.org has no pre-heading content before `* Project Alpha',
    ;; so prepend some and re-point at the file-level node.
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (insert "Top-of-file preamble.\n\n")
      (write-region (point-min) (point-max) path))
    (let* ((json (mcp-server-emacs-tools-org-get-node--handler
                  `((file . ,path))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json))))
      (should (stringp (alist-get 'body result)))
      (should (string-match-p "Top-of-file preamble\\."
                              (alist-get 'body result))))))

(ert-deftest mcp-test-org-get-node-returns-auto-created-id ()
  "When auto-id promotes a new ID, the returned alist carries it.
Regression test: the ID must appear in the serialized node, not just
the file."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (let* ((mcp-server-emacs-tools-org-auto-id t)
           ;; `Implementation' has no pre-existing ID in the fixture.
           (json (mcp-server-emacs-tools-org-get-node--handler
                  `((file . ,path)
                    (outline_path . ["Project Alpha" "Implementation"]))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json))))
      (should (stringp (alist-get 'id result)))
      (should (> (length (alist-get 'id result)) 0)))))

(provide 'test-mcp-org-get-node)
;;; test-mcp-org-get-node.el ends here
