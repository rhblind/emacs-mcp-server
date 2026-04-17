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

(provide 'test-mcp-org-clock)
;;; test-mcp-org-clock.el ends here
