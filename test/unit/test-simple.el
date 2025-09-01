;;; test-simple.el --- Simple Test to Verify Infrastructure -*- lexical-binding: t; -*-

;;; Commentary:
;; A simple test file to verify our test infrastructure works.

;;; Code:

(require 'ert)
(require 'test-helpers)

(ert-deftest mcp-test-simple-arithmetic ()
  "Test basic arithmetic to verify test infrastructure."
  (should (= (+ 1 2) 3))
  (should (= (* 3 4) 12))
  (should-not (= (+ 2 2) 5)))

(ert-deftest mcp-test-simple-strings ()
  "Test basic string operations."
  (should (equal (concat "hello" " " "world") "hello world"))
  (should (string-match-p "test" "this is a test"))
  (should-not (string-match-p "xyz" "hello world")))

(ert-deftest mcp-test-simple-lists ()
  "Test basic list operations."
  (should (equal (list 1 2 3) '(1 2 3)))
  (should (= (length '(a b c)) 3))
  (should (member 'b '(a b c))))

(ert-deftest mcp-test-temp-dir-fixture ()
  "Test temporary directory fixture."
  (mcp-test-with-temp-dir
   (should (file-directory-p mcp-test-temp-dir))
   (let ((test-file (expand-file-name "test.txt" mcp-test-temp-dir)))
     (with-temp-file test-file
       (insert "test content"))
     (should (file-exists-p test-file)))))

(ert-deftest mcp-test-json-helpers ()
  "Test JSON helper functions."
  (let* ((test-obj '((name . "test") (value . 42)))
         (json-str (mcp-test-json-encode test-obj))
         (parsed (mcp-test-json-decode json-str)))
    (should (equal (alist-get 'name parsed) "test"))
    (should (equal (alist-get 'value parsed) 42))))

(provide 'test-simple)
;;; test-simple.el ends here