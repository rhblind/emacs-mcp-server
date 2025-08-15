;;; mcp-server-simple-test.el --- Simple test examples for Emacs MCP Server -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file provides simple examples and tests for the Emacs MCP Server.

;;; Commentary:

;; This file contains examples of how to use the Emacs MCP Server
;; both interactively and programmatically.

;;; Code:

(require 'mcp-server)

;;; Interactive Testing

(defun mcp-server-test-interactive ()
  "Start the MCP server and run some interactive tests."
  (interactive)
  
  ;; Start the server with debug logging
  (mcp-server-start t)
  
  ;; Test initialization
  (mcp-server-protocol-process-input 
   "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"draft\",\"capabilities\":{},\"clientInfo\":{\"name\":\"test-client\",\"version\":\"1.0.0\"}}}\n")
  
  ;; Send initialized notification
  (mcp-server-protocol-process-input 
   "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}\n")
  
  ;; List available tools
  (mcp-server-protocol-process-input 
   "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\"}\n")
  
  ;; Test eval-elisp tool
  (mcp-server-protocol-process-input 
   "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\",\"params\":{\"name\":\"eval-elisp\",\"arguments\":{\"expression\":\"(+ 1 2 3)\"}}}\n")
  
  ;; Test get-buffer-list tool
  (mcp-server-protocol-process-input 
   "{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"tools/call\",\"params\":{\"name\":\"get-buffer-list\",\"arguments\":{}}}\n")
  
  (message "MCP server tests completed. Check *Messages* buffer for output."))

;;; Programmatic Testing

(defun mcp-server-test-tools ()
  "Test individual MCP tools programmatically."
  (interactive)
  
  ;; Initialize the tools system
  (mcp-server-tools-init)
  (mcp-server-security-init)
  (mcp-server-emacs-tools-register)
  
  ;; Test eval-elisp
  (message "Testing eval-elisp...")
  (let ((result (mcp-server-tools-call "eval-elisp" '((expression . "(* 7 6)")))))
    (message "Result: %s" result))
  
  ;; Test get-point
  (message "Testing get-point...")
  (let ((result (mcp-server-tools-call "get-point" '())))
    (message "Current point: %s" result))
  
  ;; Test get-buffer-list
  (message "Testing get-buffer-list...")
  (let ((result (mcp-server-tools-call "get-buffer-list" '())))
    (message "Buffer list: %s" result))
  
  ;; Test get-variable
  (message "Testing get-variable...")
  (let ((result (mcp-server-tools-call "get-variable" '((variable . "emacs-version")))))
    (message "Emacs version: %s" result))
  
  (message "Tool tests completed."))

;;; Security Testing

(defun mcp-server-test-security ()
  "Test security features."
  (interactive)
  
  ;; Initialize security system
  (mcp-server-security-init)
  
  ;; Test input validation
  (condition-case err
      (mcp-server-security-validate-input "safe input")
    (error (message "Unexpected error: %s" err)))
  
  ;; Test dangerous input (should fail)
  (condition-case err
      (mcp-server-security-validate-input "$(rm -rf /)")
    (error (message "Correctly caught dangerous input: %s" err)))
  
  ;; Test permission system
  (mcp-server-security-set-prompting nil)  ; Disable prompts for testing
  
  (if (mcp-server-security-check-permission 'safe-function)
      (message "Safe function correctly allowed")
    (message "Safe function incorrectly denied"))
  
  (if (mcp-server-security-check-permission 'delete-file)
      (message "Dangerous function incorrectly allowed")
    (message "Dangerous function correctly denied"))
  
  ;; Grant permission and test again
  (mcp-server-security-grant-permission 'delete-file)
  (if (mcp-server-security-check-permission 'delete-file)
      (message "Granted permission working correctly")
    (message "Granted permission not working"))
  
  (message "Security tests completed."))

;;; Performance Testing

(defun mcp-server-test-performance ()
  "Test performance of tool execution."
  (interactive)
  
  ;; Initialize systems
  (mcp-server-tools-init)
  (mcp-server-security-init)
  (mcp-server-emacs-tools-register)
  
  ;; Time tool registration
  (let ((start-time (current-time)))
    (dotimes (i 100)
      (mcp-server-tools-register
       (format "test-tool-%d" i)
       (format "Test Tool %d" i)
       "A test tool"
       '((type . "object"))
       (lambda (args) "test result")))
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      (message "Registered 100 tools in %.3f seconds" elapsed)))
  
  ;; Time tool listing
  (let ((start-time (current-time)))
    (dotimes (i 100)
      (mcp-server-tools-list))
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      (message "Listed tools 100 times in %.3f seconds" elapsed)))
  
  ;; Time tool execution
  (let ((start-time (current-time)))
    (dotimes (i 100)
      (mcp-server-tools-call "eval-elisp" '((expression . "(+ 1 1)"))))
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      (message "Executed tool 100 times in %.3f seconds" elapsed)))
  
  (message "Performance tests completed."))

;;; Example MCP Client Messages

(defconst mcp-server-test-messages
  '(;; Initialize
    "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"draft\",\"capabilities\":{\"roots\":{\"listChanged\":true}},\"clientInfo\":{\"name\":\"example-client\",\"version\":\"1.0.0\"}}}"
    
    ;; Initialized notification
    "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}"
    
    ;; List tools
    "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\"}"
    
    ;; Call eval-elisp
    "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\",\"params\":{\"name\":\"eval-elisp\",\"arguments\":{\"expression\":\"(buffer-name)\"}}}"
    
    ;; Get buffer content
    "{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"tools/call\",\"params\":{\"name\":\"get-buffer-content\",\"arguments\":{}}}"
    
    ;; Insert text
    "{\"jsonrpc\":\"2.0\",\"id\":5,\"method\":\"tools/call\",\"params\":{\"name\":\"insert-at-point\",\"arguments\":{\"text\":\"Hello from MCP!\"}}}"
    
    ;; Get point
    "{\"jsonrpc\":\"2.0\",\"id\":6,\"method\":\"tools/call\",\"params\":{\"name\":\"get-point\",\"arguments\":{}}}"
    
    ;; Get variable
    "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"tools/call\",\"params\":{\"name\":\"get-variable\",\"arguments\":{\"variable\":\"major-mode\"}}}"
    
    ;; Get buffer list
    "{\"jsonrpc\":\"2.0\",\"id\":8,\"method\":\"tools/call\",\"params\":{\"name\":\"get-buffer-list\",\"arguments\":{}}}")
  "Example MCP client messages for testing.")

(defun mcp-server-test-full-session ()
  "Run a full MCP session test with example messages."
  (interactive)
  
  ;; Start server
  (mcp-server-start t)
  
  ;; Process each message with delay
  (dolist (message mcp-server-test-messages)
    (message "Sending: %s" message)
    (mcp-server-protocol-process-input (concat message "\n"))
    (sit-for 0.1))  ; Brief pause between messages
  
  (message "Full session test completed."))

;;; Cleanup Function

(defun mcp-server-test-cleanup ()
  "Clean up after testing."
  (interactive)
  
  ;; Stop server if running
  (when mcp-server-running
    (mcp-server-stop))
  
  ;; Clear tools and security state
  (mcp-server-tools-cleanup)
  (mcp-server-security-cleanup)
  
  (message "Test cleanup completed."))

;;; Interactive Testing Menu

(defun mcp-server-test-menu ()
  "Show interactive testing menu."
  (interactive)
  (let ((choice (read-multiple-choice
                 "MCP Test Menu"
                 '((?i "interactive" "Run interactive server test")
                   (?t "tools" "Test individual tools")
                   (?s "security" "Test security features")
                   (?p "performance" "Test performance")
                   (?f "full" "Run full session test")
                   (?c "cleanup" "Clean up test state")
                   (?q "quit" "Quit menu")))))
    (pcase (car choice)
      (?i (mcp-server-test-interactive))
      (?t (mcp-server-test-tools))
      (?s (mcp-server-test-security))
      (?p (mcp-server-test-performance))
      (?f (mcp-server-test-full-session))
      (?c (mcp-server-test-cleanup))
      (?q (message "Test menu closed")))))

(provide 'mcp-server-simple-test)

;;; mcp-server-simple-test.el ends here