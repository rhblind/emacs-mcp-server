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

(provide 'test-mcp-org-common)
;;; test-mcp-org-common.el ends here
