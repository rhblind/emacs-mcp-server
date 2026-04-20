;;; test-mcp-org-common.el --- Tests for org-common -*- lexical-binding: t; -*-
(require 'ert)
(require 'test-helpers)

(let* ((this-file (or load-file-name buffer-file-name))
       (tools-dir (and this-file
                       (expand-file-name "../../tools"
                                         (file-name-directory this-file)))))
  (when tools-dir (add-to-list 'load-path tools-dir)))

(require 'mcp-server-emacs-tools-org-common)

(ert-deftest mcp-test-org-common-defcustoms-exist ()
  "Config defcustoms are defined with expected defaults."
  (should (boundp 'mcp-server-emacs-tools-org-auto-save))
  (should (eq mcp-server-emacs-tools-org-auto-save t))
  (should (boundp 'mcp-server-emacs-tools-org-auto-id))
  (should (eq mcp-server-emacs-tools-org-auto-id t))
  (should (boundp 'mcp-server-emacs-tools-org-allowed-roots))
  (should (null mcp-server-emacs-tools-org-allowed-roots))
  (should (boundp 'mcp-server-emacs-tools-org-max-body-bytes))
  (should (= mcp-server-emacs-tools-org-max-body-bytes 100000)))

(ert-deftest mcp-test-org-common-resolve-by-id ()
  "resolve-node finds a node by ID."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (let ((marker (mcp-server-emacs-tools-org-common--resolve-node
                   '((id . "alpha-design-0001")))))
      (should (markerp marker))
      (with-current-buffer (marker-buffer marker)
        (goto-char marker)
        (should (equal (org-get-heading t t t t) "Design"))))))

(ert-deftest mcp-test-org-common-resolve-by-olp ()
  "resolve-node finds a node by file + outline_path."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (let ((marker (mcp-server-emacs-tools-org-common--resolve-node
                   `((file . ,path)
                     (outline_path . ["Project Alpha" "Implementation"])))))
      (should (markerp marker))
      (with-current-buffer (marker-buffer marker)
        (goto-char marker)
        (should (equal (org-get-heading t t t t) "Implementation"))))))

(ert-deftest mcp-test-org-common-resolve-missing-id ()
  "resolve-node errors on unknown id."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (should-error
     (mcp-server-emacs-tools-org-common--resolve-node
      '((id . "does-not-exist"))))))

(ert-deftest mcp-test-org-common-resolve-missing-olp ()
  "resolve-node errors on unknown outline path."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (should-error
     (mcp-server-emacs-tools-org-common--resolve-node
      `((file . ,path)
        (outline_path . ["Project Alpha" "Nonexistent"]))))))

(ert-deftest mcp-test-org-common-resolve-requires-reference ()
  "resolve-node errors if neither id nor file is given."
  (should-error
   (mcp-server-emacs-tools-org-common--resolve-node '())))

(ert-deftest mcp-test-org-common-node-to-alist-basic ()
  "node-to-alist returns expected keys and values."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (let* ((marker (mcp-server-emacs-tools-org-common--resolve-node
                    '((id . "alpha-design-0001"))))
           (alist (mcp-server-emacs-tools-org-common--node-to-alist
                   marker :include-body t)))
      (should (equal (alist-get 'id alist) "alpha-design-0001"))
      (should (equal (alist-get 'title alist) "Design"))
      (should (equal (alist-get 'level alist) 2))
      (should (stringp (alist-get 'file alist)))
      (should (vectorp (alist-get 'outline_path alist)))
      (should (equal (append (alist-get 'outline_path alist) nil)
                     '("Project Alpha" "Design")))
      (should (string-match-p "Design notes for alpha"
                              (alist-get 'body alist))))))

(ert-deftest mcp-test-org-common-node-to-alist-omit-body ()
  "node-to-alist omits body when :include-body is nil."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (let* ((marker (mcp-server-emacs-tools-org-common--resolve-node
                    '((id . "alpha-design-0001"))))
           (alist (mcp-server-emacs-tools-org-common--node-to-alist
                   marker :include-body nil)))
      (should-not (alist-get 'body alist)))))

(ert-deftest mcp-test-org-common-node-to-alist-truncates-large-body ()
  "node-to-alist truncates body larger than max-body-bytes."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (let ((mcp-server-emacs-tools-org-max-body-bytes 10)
          (marker (mcp-server-emacs-tools-org-common--resolve-node
                   '((id . "alpha-design-0001")))))
      (let ((alist (mcp-server-emacs-tools-org-common--node-to-alist
                    marker :include-body t)))
        (should (<= (length (alist-get 'body alist)) 10))
        (should (eq (alist-get 'truncated alist) t))))))

(ert-deftest mcp-test-org-common-validate-path-within-root ()
  "validate-path accepts a path inside an allowed root."
  (let* ((tmp-root (make-temp-file "mcp-root-" t))
         (file (expand-file-name "ok.org" tmp-root))
         (mcp-server-emacs-tools-org-allowed-roots (list tmp-root)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "ok"))
          (should (mcp-server-emacs-tools-org-common--validate-path file)))
      (delete-directory tmp-root t))))

(ert-deftest mcp-test-org-common-validate-path-outside-root ()
  "validate-path rejects a path outside allowed roots."
  (let* ((tmp-root (make-temp-file "mcp-root-" t))
         (outside (make-temp-file "mcp-outside-" nil ".org"))
         (mcp-server-emacs-tools-org-allowed-roots (list tmp-root)))
    (unwind-protect
        (should-error
         (mcp-server-emacs-tools-org-common--validate-path outside))
      (delete-directory tmp-root t)
      (when (file-exists-p outside) (delete-file outside)))))

(ert-deftest mcp-test-org-common-validate-path-default-root-from-org-directory ()
  "When allowed-roots is nil, derives from org-directory."
  (let* ((tmp-root (make-temp-file "mcp-root-" t))
         (file (expand-file-name "ok.org" tmp-root))
         (org-directory tmp-root)
         (org-agenda-files nil)
         (mcp-server-emacs-tools-org-allowed-roots nil))
    (unwind-protect
        (progn
          (with-temp-file file (insert "ok"))
          (should (mcp-server-emacs-tools-org-common--validate-path file)))
      (delete-directory tmp-root t))))

(ert-deftest mcp-test-org-common-with-node-runs-body ()
  "with-node runs body with point at marker."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (let ((captured-title nil)
          (marker (mcp-server-emacs-tools-org-common--resolve-node
                   '((id . "alpha-design-0001")))))
      (mcp-server-emacs-tools-org-common--with-node marker
        (setq captured-title (org-get-heading t t t t)))
      (should (equal captured-title "Design")))))

(ert-deftest mcp-test-org-common-with-node-saves-when-auto-save ()
  "with-node saves the buffer when auto-save is t."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (let ((mcp-server-emacs-tools-org-auto-save t)
          (marker (mcp-server-emacs-tools-org-common--resolve-node
                   '((id . "alpha-design-0001")))))
      (mcp-server-emacs-tools-org-common--with-node marker
        (org-edit-headline "Design (renamed)"))
      (with-temp-buffer
        (insert-file-contents path)
        (should (string-match-p "Design (renamed)" (buffer-string)))))))

(ert-deftest mcp-test-org-common-with-node-does-not-save-when-disabled ()
  "with-node does not save when auto-save is nil."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (let ((mcp-server-emacs-tools-org-auto-save nil)
          (marker (mcp-server-emacs-tools-org-common--resolve-node
                   '((id . "alpha-design-0001")))))
      (mcp-server-emacs-tools-org-common--with-node marker
        (org-edit-headline "Not persisted"))
      (with-temp-buffer
        (insert-file-contents path)
        (should-not (string-match-p "Not persisted" (buffer-string)))))))

(ert-deftest mcp-test-org-common-promote-to-id-assigns-id ()
  "promote-to-id assigns an id to nodes that lack one."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (let* ((marker (mcp-server-emacs-tools-org-common--resolve-node
                    `((file . ,path)
                      (outline_path . ["Project Alpha" "Implementation"]))))
           (mcp-server-emacs-tools-org-auto-id t))
      (let ((id (mcp-server-emacs-tools-org-common--promote-to-id marker)))
        (should (stringp id))
        (should (> (length id) 0))))))

(ert-deftest mcp-test-org-common-augment-description-without-roam ()
  "augment-description returns base string unchanged when roam is absent."
  (cl-letf (((symbol-function 'featurep)
             (lambda (feat) (if (eq feat 'org-roam) nil (funcall #'featurep feat)))))
    (should (equal (mcp-server-emacs-tools-org-common--augment-description
                    "Search tasks."
                    'roam-hint)
                   "Search tasks."))))

(ert-deftest mcp-test-org-common-truncate-to-bytes-ascii ()
  "Byte truncation of pure ASCII matches character truncation."
  (should (equal (mcp-server-emacs-tools-org-common--truncate-to-bytes
                  "hello world" 5)
                 "hello"))
  ;; No truncation when already within the limit.
  (should (equal (mcp-server-emacs-tools-org-common--truncate-to-bytes
                  "abc" 100)
                 "abc")))

(ert-deftest mcp-test-org-common-truncate-to-bytes-multibyte ()
  "Byte truncation respects multibyte boundaries.
Regression test: previous `substring' on character length could produce
a result whose byte count exceeded the limit, or split a codepoint.
The helper must never leave the total byte length above LIMIT, and
must never split a multibyte character."
  ;; Each `é' is 2 bytes in UTF-8; `aéé' is 5 bytes.
  (let ((s "aéé"))
    (should (= (string-bytes s) 5))
    (let ((trunc (mcp-server-emacs-tools-org-common--truncate-to-bytes s 3)))
      ;; 3 bytes cannot fit the full `aéé', so we expect `aé' (3 bytes)
      ;; or `a' (1 byte), but not a half-codepoint.
      (should (<= (string-bytes trunc) 3))
      (should (or (equal trunc "aé") (equal trunc "a")))))
  ;; Limit 0 returns empty string.
  (should (equal (mcp-server-emacs-tools-org-common--truncate-to-bytes "anything" 0)
                 "")))

(provide 'test-mcp-org-common)
;;; test-mcp-org-common.el ends here
