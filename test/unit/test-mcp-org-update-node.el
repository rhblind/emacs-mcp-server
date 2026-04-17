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

(ert-deftest mcp-test-org-update-node-replace-tags ()
  "`tags' replaces existing tag list entirely."
  (mcp-test-with-org-fixture "sample-agenda.org" path
    (mcp-server-emacs-tools-org-update-node--handler
     '((id . "agenda-task-0003")
       (tags . ["archived"])))
    (with-temp-buffer
      (insert-file-contents path)
      (let ((buf (buffer-string)))
        (should (string-match-p ":archived:" buf))
        (should-not (string-match-p ":work:release:" buf))
        (should-not (string-match-p ":release:" buf))))))

(ert-deftest mcp-test-org-update-node-remove-tags ()
  "`remove_tags' drops only the listed tags."
  (mcp-test-with-org-fixture "sample-agenda.org" path
    (mcp-server-emacs-tools-org-update-node--handler
     '((id . "agenda-task-0003")
       (remove_tags . ["release"])))
    (with-temp-buffer
      (insert-file-contents path)
      (let ((buf (buffer-string)))
        (should (string-match-p ":work:" buf))
        (should-not (string-match-p ":release:" buf))))))

(ert-deftest mcp-test-org-update-node-set-priority ()
  "`priority' sets the heading priority cookie."
  (mcp-test-with-org-fixture "sample-agenda.org" path
    (mcp-server-emacs-tools-org-update-node--handler
     '((id . "agenda-task-0001")
       (priority . "A")))
    (with-temp-buffer
      (insert-file-contents path)
      (should (string-match-p "\\*+ TODO \\[#A\\] Write spec" (buffer-string))))))

(ert-deftest mcp-test-org-update-node-set-properties ()
  "`properties' adds arbitrary properties to the drawer."
  (mcp-test-with-org-fixture "sample-agenda.org" path
    (mcp-server-emacs-tools-org-update-node--handler
     '((id . "agenda-task-0001")
       (properties . ((CATEGORY . "planning")
                      (EFFORT . "30")))))
    (with-temp-buffer
      (insert-file-contents path)
      (let ((buf (buffer-string)))
        (should (string-match-p ":CATEGORY: +planning" buf))
        (should (string-match-p ":EFFORT: +30" buf))))))

(ert-deftest mcp-test-org-update-node-remove-properties ()
  "`remove_properties' deletes named properties."
  (mcp-test-with-org-fixture "sample-agenda.org" path
    ;; First add a property, then remove it.
    (mcp-server-emacs-tools-org-update-node--handler
     '((id . "agenda-task-0001")
       (properties . ((CATEGORY . "temp")))))
    (mcp-server-emacs-tools-org-update-node--handler
     '((id . "agenda-task-0001")
       (remove_properties . ["CATEGORY"])))
    (with-temp-buffer
      (insert-file-contents path)
      (should-not (string-match-p ":CATEGORY:" (buffer-string))))))

(ert-deftest mcp-test-org-update-node-set-scheduled ()
  "`scheduled' adds a SCHEDULED line."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (mcp-server-emacs-tools-org-update-node--handler
     '((id . "alpha-design-0001")
       (scheduled . "2026-05-01 Fri")))
    (with-temp-buffer
      (insert-file-contents path)
      (should (string-match-p "SCHEDULED: <2026-05-01" (buffer-string))))))

(ert-deftest mcp-test-org-update-node-clear-scheduled ()
  "`scheduled' set to null removes the SCHEDULED line."
  (mcp-test-with-org-fixture "sample-agenda.org" path
    ;; agenda-task-0001 starts with SCHEDULED set.
    (mcp-server-emacs-tools-org-update-node--handler
     '((id . "agenda-task-0001")
       (scheduled . nil)))
    (with-temp-buffer
      (insert-file-contents path)
      (should-not (string-match-p "SCHEDULED: <2026-04-18" (buffer-string))))))

(ert-deftest mcp-test-org-update-node-set-deadline ()
  "`deadline' adds a DEADLINE line."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (mcp-server-emacs-tools-org-update-node--handler
     '((id . "alpha-design-0001")
       (deadline . "2026-06-15 Mon")))
    (with-temp-buffer
      (insert-file-contents path)
      (should (string-match-p "DEADLINE: <2026-06-15" (buffer-string))))))

(ert-deftest mcp-test-org-update-node-changed-field-echoed ()
  "Handler returns the list of fields it modified."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (let* ((json (mcp-server-emacs-tools-org-update-node--handler
                  '((id . "alpha-design-0001")
                    (priority . "B")
                    (add_tags . ["review"]))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json)))
           (changed (append (alist-get 'changed result) nil)))
      (should (member "priority" changed))
      (should (member "tags" changed)))))

(provide 'test-mcp-org-update-node)
;;; test-mcp-org-update-node.el ends here
