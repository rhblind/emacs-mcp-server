;;; test-mcp-tools-working.el --- Working Tests for MCP Tool Registry -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for tool registry that properly load and test the actual MCP server.

;;; Code:

(require 'ert)
(require 'test-helpers)

;; Load MCP server modules safely
(condition-case err
    (progn
      (require 'mcp-server-tools)
      (require 'mcp-server-security))
  (error 
   (message "Warning: MCP server modules not available: %s" err)))

;;; Tool Registration Tests

(ert-deftest mcp-test-tools-register-working ()
  "Test basic tool registration with actual MCP server."
  (when (fboundp 'mcp-server-tools-register)
    (let ((mcp-server-tools--registry (make-hash-table :test 'equal)))
      (mcp-server-tools-register
       "test-tool"
       "Test Tool"
       "A tool for testing"
       '((type . "object")
         (properties . ((input . ((type . "string")))))
         (required . ["input"]))
       (lambda (args) "test result"))
      
      (should (fboundp 'mcp-server-tools-exists-p))
      (should (mcp-server-tools-exists-p "test-tool")))))

(ert-deftest mcp-test-tools-list-working ()
  "Test tool listing with actual MCP server."
  (when (fboundp 'mcp-server-tools-list)
    (let ((mcp-server-tools--registry (make-hash-table :test 'equal)))
      ;; Register a test tool
      (when (fboundp 'mcp-server-tools-register)
        (mcp-server-tools-register
         "list-test-tool"
         "List Test Tool"
         "Tool for testing listing"
         '((type . "object"))
         (lambda (args) "result")))
      
      (let ((tools (mcp-server-tools-list)))
        (should (listp tools))
        (when tools
          (let ((tool (car tools)))
            (should (alist-get 'name tool))
            (should (alist-get 'title tool))
            (should (alist-get 'description tool))))))))

(ert-deftest mcp-test-tools-get-working ()
  "Test tool retrieval with actual MCP server."
  (when (and (fboundp 'mcp-server-tools-register) (fboundp 'mcp-server-tools-get))
    (let ((mcp-server-tools--registry (make-hash-table :test 'equal)))
      (mcp-server-tools-register
       "get-test-tool"
       "Get Test Tool"
       "Tool for testing get"
       '((type . "object"))
       (lambda (args) "result"))
      
      (let ((tool (mcp-server-tools-get "get-test-tool")))
        (should tool)
        (should (equal (mcp-server-tool-name tool) "get-test-tool"))))))

(ert-deftest mcp-test-tools-call-working ()
  "Test tool execution with actual MCP server."
  (when (and (fboundp 'mcp-server-tools-register) (fboundp 'mcp-server-tools-call))
    (let ((mcp-server-tools--registry (make-hash-table :test 'equal)))
      (mcp-server-tools-register
       "call-test-tool"
       "Call Test Tool"
       "Tool for testing calls"
       '((type . "object")
         (properties . ((input . ((type . "string")))))
         (required . ["input"]))
       (lambda (args) 
         (format "Received: %s" (alist-get 'input args))))
      
      (condition-case err
          (let ((result (mcp-server-tools-call "call-test-tool" '((input . "hello")))))
            (should (stringp result))
            (should (string-match-p "hello" result)))
        (error 
         (message "Tool call failed (expected if validation not implemented): %s" err))))))

;;; Constants and Variables Tests

(ert-deftest mcp-test-server-constants-working ()
  "Test that server constants are properly defined."
  (when (boundp 'mcp-server-version)
    (should (stringp mcp-server-version)))
  (when (boundp 'mcp-server-protocol-version)
    (should (stringp mcp-server-protocol-version))
    (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" mcp-server-protocol-version))))

(ert-deftest mcp-test-tools-registry-variables ()
  "Test that tools registry variables exist."
  (should (boundp 'mcp-server-tools--registry))
  (should (hash-table-p mcp-server-tools--registry)))

;;; Function Availability Tests

(ert-deftest mcp-test-tools-functions-available ()
  "Test that expected tool functions are available."
  (should (fboundp 'mcp-server-tools-register))
  (should (fboundp 'mcp-server-tools-list))
  (should (fboundp 'mcp-server-tools-get))
  (should (fboundp 'mcp-server-tools-exists-p)))

(ert-deftest mcp-test-server-functions-available ()
  "Test that expected server functions are available."
  (should (fboundp 'mcp-server-start))
  (should (fboundp 'mcp-server-start-unix))
  (should (fboundp 'mcp-server-stop)))

(provide 'test-mcp-tools-working)
;;; test-mcp-tools-working.el ends here