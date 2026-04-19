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

(ert-deftest mcp-test-org-update-node-clear-all-tags-via-empty-array ()
  "`tags' set to an empty array clears all existing tags.
Regression test: the handler previously skipped the tag update block
when the value was an empty list because it treated presence as
truthiness.  Explicit empty-list input must now clear tags."
  (mcp-test-with-org-fixture "sample-agenda.org" path
    ;; agenda-task-0003 starts with :work:release:
    (mcp-server-emacs-tools-org-update-node--handler
     '((id . "agenda-task-0003")
       (tags . [])))
    (with-temp-buffer
      (insert-file-contents path)
      (let ((line (save-excursion
                    (goto-char (point-min))
                    (re-search-forward "^\\*+ .*Release v0\\.5\\.0.*$" nil t)
                    (match-string 0))))
        (should line)
        (should-not (string-match-p ":work:" line))
        (should-not (string-match-p ":release:" line))))))

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
  "`scheduled' set to null (elisp nil) removes the SCHEDULED line."
  (mcp-test-with-org-fixture "sample-agenda.org" path
    ;; agenda-task-0001 starts with SCHEDULED set.
    (mcp-server-emacs-tools-org-update-node--handler
     '((id . "agenda-task-0001")
       (scheduled . nil)))
    (with-temp-buffer
      (insert-file-contents path)
      (should-not (string-match-p "SCHEDULED: <2026-04-18" (buffer-string))))))

(ert-deftest mcp-test-org-update-node-clear-scheduled-via-json-null ()
  "`scheduled' set to JSON null (:null after json-parse-string) clears it.
Regression test: real MCP calls arrive with `:null', not elisp nil."
  (mcp-test-with-org-fixture "sample-agenda.org" path
    (mcp-server-emacs-tools-org-update-node--handler
     '((id . "agenda-task-0001")
       (scheduled . :null)))
    (with-temp-buffer
      (insert-file-contents path)
      (should-not (string-match-p "SCHEDULED: <2026-04-18" (buffer-string))))))

(ert-deftest mcp-test-org-update-node-clear-deadline-via-json-null ()
  "`deadline' set to JSON null (:null) clears it."
  (mcp-test-with-org-fixture "sample-agenda.org" path
    ;; agenda-task-0002 starts with DEADLINE set.
    (mcp-server-emacs-tools-org-update-node--handler
     '((id . "agenda-task-0002")
       (deadline . :null)))
    (with-temp-buffer
      (insert-file-contents path)
      (should-not (string-match-p "DEADLINE: <2026-04-20" (buffer-string))))))

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
