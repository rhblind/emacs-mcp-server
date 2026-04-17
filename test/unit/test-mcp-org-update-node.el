;;; test-mcp-org-update-node.el --- Tests for org-update-node -*- lexical-binding: t; -*-
(require 'ert)
(require 'test-helpers)
(require 'json)

(let* ((this-file (or load-file-name buffer-file-name))
       (tools-dir (and this-file
                       (expand-file-name "../../tools"
                                         (file-name-directory this-file)))))
  (when tools-dir (add-to-list 'load-path tools-dir)))

(require 'mcp-server-emacs-tools-org-update-node)

(ert-deftest mcp-test-org-update-node-change-title ()
  "update-node changes the heading title."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (let* ((json (mcp-server-emacs-tools-org-update-node--handler
                  '((id . "alpha-design-0001")
                    (title . "Architecture plan"))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json))))
      (should (member "title" (append (alist-get 'changed result) nil)))
      (with-temp-buffer
        (insert-file-contents path)
        (should (string-match-p "Architecture plan" (buffer-string)))
        (should-not (string-match-p "^\\*+ Design$" (buffer-string)))))))

(ert-deftest mcp-test-org-update-node-add-tags ()
  "update-node adds tags without removing existing ones."
  (mcp-test-with-org-fixture "sample-agenda.org" path
    (mcp-server-emacs-tools-org-update-node--handler
     '((id . "agenda-task-0002")
       (add_tags . ["urgent"])))
    (with-temp-buffer
      (insert-file-contents path)
      (should (string-match-p ":urgent:" (buffer-string)))
      (should (string-match-p ":work:" (buffer-string))))))

(ert-deftest mcp-test-org-update-node-set-todo ()
  "update-node changes the TODO state."
  (mcp-test-with-org-fixture "sample-agenda.org" path
    (mcp-server-emacs-tools-org-update-node--handler
     '((id . "agenda-task-0001")
       (todo_state . "DONE")))
    (with-temp-buffer
      (insert-file-contents path)
      (should (string-match-p "\\*+ DONE Write spec" (buffer-string))))))

(ert-deftest mcp-test-org-update-node-replace-body-preserves-properties ()
  "update-node replaces body but leaves :PROPERTIES: intact."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (mcp-server-emacs-tools-org-update-node--handler
     '((id . "alpha-design-0001")
       (body . "New body text.\n")))
    (with-temp-buffer
      (insert-file-contents path)
      (let ((buf (buffer-string)))
        (should (string-match-p "New body text\\." buf))
        (should (string-match-p ":ID:       alpha-design-0001" buf))
        (should-not (string-match-p "Design notes for alpha\\." buf))
        (should (string-match-p "\\*\\*\\* Architecture" buf))))))

(ert-deftest mcp-test-org-update-node-registered ()
  "Tool is registered."
  (should (mcp-server-tools-exists-p "org-update-node")))

(provide 'test-mcp-org-update-node)
;;; test-mcp-org-update-node.el ends here
