;;; test-config.el --- Test configuration for MCP Server -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file provides test configuration and utilities for the MCP Server.

;;; Code:

;; Add the current directory to load path
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

;; Load the MCP server
(require 'mcp-server)

;; Test configuration variables
(defvar mcp-test-socket-name "test-instance"
  "Socket name to use for testing.")

(defvar mcp-test-debug t
  "Whether to enable debug logging during tests.")

(defvar mcp-test-timeout 10
  "Timeout for test operations in seconds.")

;; Test utility functions
(defun mcp-test-setup ()
  "Set up MCP server for testing."
  (interactive)
  (message "Setting up MCP server for testing...")
  
  ;; Configure socket naming for predictable testing
  (setq mcp-server-socket-name mcp-test-socket-name)
  (setq mcp-server-debug mcp-test-debug)
  
  (message "Test configuration:")
  (message "  Socket name: %s" mcp-server-socket-name)
  (message "  Debug enabled: %s" mcp-server-debug)
  (message "  Expected socket path: %s" 
           (let ((expected-path (mcp-server-get-predicted-socket-path)))
             expected-path)))

(defun mcp-test-start-server ()
  "Start MCP server with test configuration."
  (interactive)
  (mcp-test-setup)
  
  (message "Starting MCP server...")
  (condition-case err
      (progn
        (mcp-server-start-unix mcp-test-debug)
        (message "MCP server started successfully!")
        (mcp-server-status)
        
        ;; Show socket path
        (let ((socket-path (mcp-server-get-socket-path)))
          (message "Socket path: %s" socket-path)
          socket-path))
    (error
     (message "Failed to start MCP server: %s" (error-message-string err))
     nil)))

(defun mcp-test-stop-server ()
  "Stop MCP server."
  (interactive)
  (condition-case err
      (progn
        (mcp-server-stop)
        (message "MCP server stopped."))
    (error
     (message "Error stopping server: %s" (error-message-string err)))))

(defun mcp-test-restart-server ()
  "Restart MCP server with test configuration."
  (interactive)
  (mcp-test-stop-server)
  (sleep-for 1)
  (mcp-test-start-server))

(defun mcp-test-server-info ()
  "Display comprehensive server information."
  (interactive)
  (message "=== MCP Server Test Information ===")
  (mcp-server-show-socket-config)
  (mcp-server-status)
  (when (and mcp-server-running 
             (string= mcp-server-current-transport "unix"))
    (let ((socket-path (mcp-server-get-socket-path)))
      (message "Socket exists: %s" (file-exists-p socket-path))
      (when (file-exists-p socket-path)
        (message "Socket permissions: %s" (file-modes socket-path)))))
  (mcp-server-list-clients))

(defun mcp-test-run-batch ()
  "Run MCP server in batch mode for testing."
  (mcp-test-setup)
  
  ;; Start server
  (message "Starting MCP server in batch mode...")
  (mcp-server-start-unix mcp-test-debug)
  
  ;; Show status
  (mcp-test-server-info)
  
  ;; Keep running for testing
  (message "MCP server ready for testing. Use Ctrl+C to stop.")
  (while mcp-server-running
    (sleep-for 1)))

(defun mcp-test-validate-refactoring ()
  "Validate function and variable naming consistency.
This function checks that all expected function and variable names exist
and that no deprecated names remain from previous package versions."
  (interactive)
  (message "=== Validating MCP Server Function Names ===")
  
  ;; Test that deprecated function names don't exist
  (let ((deprecated-functions '(emacs-mcp-server-start
                                emacs-mcp-server-stop
                                emacs-mcp-server-status
                                emacs-mcp-server-start-unix)))
    (dolist (func deprecated-functions)
      (if (fboundp func)
          (message "WARNING: Deprecated function still exists: %s" func)
        (message "✓ Deprecated function not found: %s" func))))
  
  ;; Test that current function names exist
  (let ((current-functions '(mcp-server-start
                             mcp-server-stop
                             mcp-server-status
                             mcp-server-start-unix
                             mcp-server-set-socket-name
                             mcp-server-show-socket-config)))
    (dolist (func current-functions)
      (if (fboundp func)
          (message "✓ Function exists: %s" func)
        (message "ERROR: Function missing: %s" func))))
  
  ;; Test variables
  (let ((expected-variables '(mcp-server-socket-name
                              mcp-server-debug
                              mcp-server-running
                              mcp-server-current-transport)))
    (dolist (var expected-variables)
      (if (boundp var)
          (message "✓ Variable exists: %s = %s" var (symbol-value var))
        (message "ERROR: Variable missing: %s" var))))
  
  (message "=== Function name validation complete ==="))

;; Interactive test commands
(defun mcp-test-interactive ()
  "Start interactive testing session."
  (interactive)
  (message "=== MCP Server Interactive Testing ===")
  (message "Available commands:")
  (message "  M-x mcp-test-setup        - Configure test settings") 
  (message "  M-x mcp-test-start-server - Start server")
  (message "  M-x mcp-test-stop-server  - Stop server")
  (message "  M-x mcp-test-server-info  - Show server information")
  (message "  M-x mcp-test-validate-refactoring - Validate function names")
  (message "")
  (message "Run the start command, then use external test scripts to validate.")
  (mcp-test-validate-refactoring))

(provide 'test-config)

;;; test-config.el ends here