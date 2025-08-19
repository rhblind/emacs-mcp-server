;;; sample-elisp-expressions.el --- Sample Elisp Expressions for Testing -*- lexical-binding: t; -*-

;;; Commentary:
;; Sample Elisp expressions for testing the eval-elisp tool and security layer.

;;; Code:

;; Safe expressions that should always be allowed
(defvar mcp-test-safe-expressions
  '((+ 1 2 3)
    (concat "hello" " " "world")
    (length '(a b c d e))
    (mapcar #'upcase '("foo" "bar" "baz"))
    (let ((x 10) (y 20)) (+ x y))
    (if (> 5 3) "greater" "lesser")
    (format "The answer is %d" 42)
    (substring "hello world" 0 5)
    (string-match "test" "this is a test")
    (split-string "a,b,c" ","))
  "List of safe Elisp expressions for testing.")

;; Dangerous expressions that should require permission
(defvar mcp-test-dangerous-expressions
  '((delete-file "/tmp/test.txt")
    (shell-command "ls -la")
    (find-file "/etc/passwd")
    (save-buffer)
    (kill-emacs)
    (call-process "echo" nil t nil "hello")
    (browse-url "http://example.com")
    (require 'some-package)
    (load "dangerous-file.el")
    (eval '(delete-file "/important-file")))
  "List of dangerous Elisp expressions for testing.")

;; Complex expressions for advanced testing
(defvar mcp-test-complex-expressions
  '((defun test-function (x y) (+ x y))
    (let ((nums '(1 2 3 4 5)))
      (mapcar (lambda (n) (* n n)) nums))
    (condition-case err
         (/ 1 0)
       (error (format "Caught error: %s" err)))
    (with-temp-buffer
      (insert "test content")
      (buffer-string))
    (save-excursion
      (goto-char (point-min))
      (search-forward "test" nil t))
    (cl-loop for i from 1 to 10
             collect (* i i)))
  "List of complex Elisp expressions for testing.")

;; Malformed expressions that should cause parse errors
(defvar mcp-test-malformed-expressions
  '("(+ 1 2"
    "(incomplete"
    ")"
    "(undefined-function)"
    "(let ((x)) (+ x y))"
    "(if true)"
    "(lambda)")
  "List of malformed expressions for error testing.")

;; Expressions that should timeout
(defvar mcp-test-timeout-expressions
  '((while t (sleep-for 0.1))
    (dotimes (i 100000000) i)
    (let ((result 0))
      (while (< result 1000000)
        (setq result (1+ result)))
      result))
  "List of expressions that should timeout for testing execution limits.")

;; Expressions with side effects (should be handled carefully)
(defvar mcp-test-side-effect-expressions
  '((message "This is a test message")
    (setq test-variable "test-value")
    (with-current-buffer (get-buffer-create "*test*")
      (insert "test content"))
    (push "test-item" test-list)
    (add-to-list 'auto-mode-alist '("\\.test\\'" . text-mode)))
  "List of expressions with side effects for testing.")

;; Expressions for testing different data types
(defvar mcp-test-data-type-expressions
  '((list 'a 'b 'c)
    (vector 1 2 3 4)
    (make-hash-table :test 'equal)
    "string-literal"
    42
    3.14159
    t
    nil
    'symbol
    :keyword)
  "List of expressions testing different data types.")

(provide 'sample-elisp-expressions)
;;; sample-elisp-expressions.el ends here