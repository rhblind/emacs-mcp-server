;;; test-mcp-org-refile.el --- Tests for org-refile -*- lexical-binding: t; -*-
(require 'ert)
(require 'test-helpers)
(require 'json)

(let* ((this-file (or load-file-name buffer-file-name))
       (tools-dir (and this-file
                       (expand-file-name "../../tools"
                                         (file-name-directory this-file)))))
  (when tools-dir (add-to-list 'load-path tools-dir)))

(require 'mcp-server-emacs-tools-org-refile)

(ert-deftest mcp-test-org-refile-moves-heading ()
  "refile moves source heading under target."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (mcp-server-emacs-tools-org-refile--handler
     '((source . ((id . "alpha-design-0001")))
       (target . ((id . "beta-root-0001")))))
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (search-forward "Project Beta")
      (search-forward "Design")
      (should t))))

(ert-deftest mcp-test-org-refile-registered ()
  "Tool is registered."
  (should (mcp-server-tools-exists-p "org-refile")))

(ert-deftest mcp-test-org-refile-rejects-descendant-target ()
  "refile errors when target is inside source subtree."
  (mcp-test-with-org-fixture "sample-notes.org" path
    ;; Move `Project Alpha' (which contains `Design') into `Design'.
    ;; That would place Alpha inside its own subtree -- must fail.
    (let* ((json (mcp-server-emacs-tools-org-refile--handler
                  '((source . ((id . "alpha-root-0001")))
                    (target . ((id . "alpha-design-0001"))))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json))))
      (should (alist-get 'error result)))))

(ert-deftest mcp-test-org-refile-by-olp ()
  "refile also works when source/target are specified by outline_path."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (mcp-server-emacs-tools-org-refile--handler
     `((source . ((file . ,path) (outline_path . ["Project Alpha" "Implementation"])))
       (target . ((id . "beta-root-0001")))))
    (with-temp-buffer
      (insert-file-contents path)
      (let ((buf (buffer-string)))
        ;; "Implementation" should now be under Project Beta.
        ;; Find it after "Project Beta" heading.
        (should (string-match-p "\\*+ Project Beta[^*]*\\*+ Implementation" buf))))))

(ert-deftest mcp-test-org-refile-rejects-file-level-target ()
  "refile errors when target is specified as a file with no outline_path.
Regression test: previously `target-heading' could be nil and the
internal `org-refile' call would fail with an obscure error.  Refile
now surfaces a clean error from the heading-only resolver."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (let* ((json (mcp-server-emacs-tools-org-refile--handler
                  `((source . ((id . "alpha-design-0001")))
                    (target . ((file . ,path))))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json))))
      (should (alist-get 'error result)))))

(provide 'test-mcp-org-refile)
;;; test-mcp-org-refile.el ends here
