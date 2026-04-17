;;; bootstrap-elpa.el --- Load test-local ELPA packages -*- lexical-binding: t; -*-

;;; Commentary:

;; When a test-local ELPA cache exists at `test/.elpa/', activate it so
;; optional test dependencies (such as `org-roam' on Emacs 29+) are
;; visible on `load-path'.  No-op when the cache is absent.

;;; Code:

(require 'package)

(let* ((this-file (or load-file-name buffer-file-name))
       (elpa-dir (and this-file
                      (expand-file-name "../.elpa"
                                        (file-name-directory this-file)))))
  (when (and elpa-dir (file-directory-p elpa-dir))
    (setq package-user-dir elpa-dir)
    (package-initialize)))

(provide 'bootstrap-elpa)

;;; bootstrap-elpa.el ends here
