;;; test-mcp-org-roam-search.el --- Tests for org-roam-search -*- lexical-binding: t; -*-
(require 'ert)
(require 'test-helpers)
(require 'json)

(let* ((this-file (or load-file-name buffer-file-name))
       (tools-dir (and this-file
                       (expand-file-name "../../tools"
                                         (file-name-directory this-file)))))
  (when tools-dir (add-to-list 'load-path tools-dir)))

(require 'mcp-server-emacs-tools-org-roam-search)

(defconst mcp-test-roam-available
  (and (>= emacs-major-version 29)
       (require 'org-roam nil t))
  "Non-nil when org-roam can be tested (Emacs 29+ with roam loadable).")

(ert-deftest mcp-test-org-roam-search-absent-when-roam-missing ()
  "Tool does not register when org-roam is missing."
  (unless (featurep 'org-roam)
    (should-not (mcp-server-tools-exists-p "org-roam-search"))))

(ert-deftest mcp-test-org-roam-search-present-when-roam-loaded ()
  "Tool registers when org-roam is loaded."
  (when (featurep 'org-roam)
    (should (mcp-server-tools-exists-p "org-roam-search"))))

(ert-deftest mcp-test-org-roam-search-by-query ()
  "Search by title substring returns matching nodes."
  (skip-unless mcp-test-roam-available)
  (mcp-test-with-roam-fixture dir
    (let* ((json (mcp-server-emacs-tools-org-roam-search--handler
                  '((query . "Concept"))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json)))
           (results (alist-get 'results result))
           (titles (mapcar (lambda (r) (alist-get 'title r)) (append results nil))))
      (should (member "Concept A" titles))
      (should (member "Concept B" titles))
      (should-not (member "Project Notes" titles)))))

(ert-deftest mcp-test-org-roam-search-by-tag ()
  "Search by tag returns nodes with that tag."
  (skip-unless mcp-test-roam-available)
  (mcp-test-with-roam-fixture dir
    (let* ((json (mcp-server-emacs-tools-org-roam-search--handler
                  '((tags . ["example"]))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json)))
           (results (alist-get 'results result))
           (titles (mapcar (lambda (r) (alist-get 'title r)) (append results nil))))
      (should (member "Concept A" titles))
      (should (member "Concept B" titles))
      (should-not (member "Project Notes" titles)))))

(ert-deftest mcp-test-org-roam-search-limit-truncates ()
  "Limit caps result count and sets truncated flag."
  (skip-unless mcp-test-roam-available)
  (mcp-test-with-roam-fixture dir
    (let* ((json (mcp-server-emacs-tools-org-roam-search--handler
                  '((query . "") (limit . 1))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json))))
      (should (= (length (alist-get 'results result)) 1))
      (should (eq (alist-get 'truncated result) t)))))

(ert-deftest mcp-test-org-roam-search-accepts-float-limit ()
  "`limit' accepts a JSON number that parses as a float.
Regression test: internal counter/comparisons must coerce to integer."
  (skip-unless mcp-test-roam-available)
  (mcp-test-with-roam-fixture dir
    (let* ((json (mcp-server-emacs-tools-org-roam-search--handler
                  '((query . "") (limit . 1.0))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json))))
      (should (= (length (alist-get 'results result)) 1))
      (should (eq (alist-get 'truncated result) t)))))

(ert-deftest mcp-test-org-roam-search-filters-files-outside-root ()
  "Nodes whose file is outside allowed roots are silently filtered out.
Regression test: `file' values from the roam DB are validated against
`mcp-server-emacs-tools-org-allowed-roots'."
  (skip-unless mcp-test-roam-available)
  (mcp-test-with-roam-fixture dir
    ;; Tighten allowed-roots to another temp dir that the fixture isn't in.
    (let ((mcp-server-emacs-tools-org-allowed-roots
           (list (make-temp-file "mcp-other-root-" t))))
      (let* ((json (mcp-server-emacs-tools-org-roam-search--handler
                    '((query . "Concept"))))
             (result (let ((json-object-type 'alist)) (json-read-from-string json))))
        (should (= (length (alist-get 'results result)) 0))))))

(provide 'test-mcp-org-roam-search)
;;; test-mcp-org-roam-search.el ends here
