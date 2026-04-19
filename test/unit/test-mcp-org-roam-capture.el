;;; test-mcp-org-roam-capture.el --- Tests for org-roam-capture -*- lexical-binding: t; -*-
(require 'ert)
(require 'test-helpers)
(require 'json)

(let* ((this-file (or load-file-name buffer-file-name))
       (tools-dir (and this-file
                       (expand-file-name "../../tools"
                                         (file-name-directory this-file)))))
  (when tools-dir (add-to-list 'load-path tools-dir)))

(require 'mcp-server-emacs-tools-org-roam-capture)

(defconst mcp-test-roam-available
  (and (>= emacs-major-version 29)
       (require 'org-roam nil t)))

(ert-deftest mcp-test-org-roam-capture-registration-matches-availability ()
  "Tool registration matches org-roam availability."
  (if (featurep 'org-roam)
      (should (mcp-server-tools-exists-p "org-roam-capture"))
    (should-not (mcp-server-tools-exists-p "org-roam-capture"))))

(ert-deftest mcp-test-org-roam-capture-direct-creates-node ()
  "Direct mode creates a node with given title and body."
  (skip-unless mcp-test-roam-available)
  (mcp-test-with-roam-fixture dir
    (let* ((json (mcp-server-emacs-tools-org-roam-capture--handler
                  '((title . "Fresh Node")
                    (body . "Some body content."))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json))))
      (should (alist-get 'id result))
      (should (alist-get 'file result))
      (should (equal (alist-get 'title result) "Fresh Node"))
      (let ((created-file (alist-get 'file result)))
        (should (file-exists-p created-file))
        (with-temp-buffer
          (insert-file-contents created-file)
          (should (string-match-p "Fresh Node" (buffer-string)))
          (should (string-match-p "Some body content" (buffer-string))))))))

(ert-deftest mcp-test-org-roam-capture-direct-with-aliases-and-refs ()
  "Direct mode preserves ALL aliases and refs (regression test for overwrite bug)."
  (skip-unless mcp-test-roam-available)
  (mcp-test-with-roam-fixture dir
    (let* ((json (mcp-server-emacs-tools-org-roam-capture--handler
                  '((title . "Multi")
                    (aliases . ["Alt1" "Alt2"])
                    (refs . ["https://example.com/x" "https://example.com/y"]))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json)))
           (file (alist-get 'file result)))
      (should file)
      (with-temp-buffer
        (insert-file-contents file)
        (let ((buf (buffer-string)))
          ;; Both aliases preserved (space-separated in the property).
          (should (string-match-p "Alt1" buf))
          (should (string-match-p "Alt2" buf))
          ;; Both refs preserved.
          (should (string-match-p "example.com/x" buf))
          (should (string-match-p "example.com/y" buf)))))))

(ert-deftest mcp-test-org-roam-capture-direct-with-tags ()
  "Direct mode writes file-level tags via `#+filetags:' on a title-only note.
Regression test: the previous implementation searched for `^\\*' and
called `org-set-tags' off-heading, which errors when the synthetic
template produced only a `#+title:' line."
  (skip-unless mcp-test-roam-available)
  (mcp-test-with-roam-fixture dir
    (let* ((json (mcp-server-emacs-tools-org-roam-capture--handler
                  '((title . "Tagged Node")
                    (tags . ["tagone" "tagtwo"]))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json)))
           (file (alist-get 'file result)))
      (should file)
      (with-temp-buffer
        (insert-file-contents file)
        (let ((buf (buffer-string)))
          (should (string-match-p "^#\\+filetags:.*:tagone:" buf))
          (should (string-match-p "^#\\+filetags:.*:tagtwo:" buf)))))))

(provide 'test-mcp-org-roam-capture)
;;; test-mcp-org-roam-capture.el ends here
