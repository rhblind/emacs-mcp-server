;;; test-mcp-org-clock.el --- Tests for org-clock -*- lexical-binding: t; -*-
(require 'ert)
(require 'test-helpers)
(require 'json)

(let* ((this-file (or load-file-name buffer-file-name))
       (tools-dir (and this-file
                       (expand-file-name "../../tools"
                                         (file-name-directory this-file)))))
  (when tools-dir (add-to-list 'load-path tools-dir)))

(require 'mcp-server-emacs-tools-org-clock)

(ert-deftest mcp-test-org-clock-in-and-out ()
  "clock action=in starts a clock, action=out stops it."
  (mcp-test-with-org-fixture "sample-agenda.org" path
    (let* ((in-json (mcp-server-emacs-tools-org-clock--handler
                     '((action . "in") (id . "agenda-task-0001"))))
           (in-result (let ((json-object-type 'alist)) (json-read-from-string in-json))))
      (should (equal (alist-get 'action in-result) "in"))
      (should (equal (alist-get 'clocked_id in-result) "agenda-task-0001")))
    (let* ((out-json (mcp-server-emacs-tools-org-clock--handler
                      '((action . "out"))))
           (out-result (let ((json-object-type 'alist)) (json-read-from-string out-json))))
      (should (equal (alist-get 'action out-result) "out")))))

(ert-deftest mcp-test-org-clock-registered ()
  "Tool is registered."
  (should (mcp-server-tools-exists-p "org-clock")))

(ert-deftest mcp-test-org-clock-cancel ()
  "clock cancel discards the running clock."
  (mcp-test-with-org-fixture "sample-agenda.org" path
    ;; Start a clock.
    (mcp-server-emacs-tools-org-clock--handler
     '((action . "in") (id . "agenda-task-0001")))
    ;; Cancel it.
    (let* ((json (mcp-server-emacs-tools-org-clock--handler
                  '((action . "cancel"))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json))))
      (should (equal (alist-get 'action result) "cancel")))
    ;; No CLOCK line should have been written on cancel
    ;; (cancel discards unlike out which records duration).
    (with-temp-buffer
      (insert-file-contents path)
      (should-not (string-match-p "CLOCK: \\[20" (buffer-string))))))

(ert-deftest mcp-test-org-clock-unknown-action ()
  "clock with unknown action returns an error."
  (let* ((json (mcp-server-emacs-tools-org-clock--handler
                '((action . "freeze"))))
         (result (let ((json-object-type 'alist)) (json-read-from-string json))))
    (should (alist-get 'error result))))

(ert-deftest mcp-test-org-clock-in-requires-reference ()
  "clock in without id/file signals error."
  (let* ((json (mcp-server-emacs-tools-org-clock--handler
                '((action . "in"))))
         (result (let ((json-object-type 'alist)) (json-read-from-string json))))
    (should (alist-get 'error result))))

(provide 'test-mcp-org-clock)
;;; test-mcp-org-clock.el ends here
