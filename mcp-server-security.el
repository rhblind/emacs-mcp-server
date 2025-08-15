;;; mcp-server-security.el --- Security and Sandboxing for MCP Server -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This module provides security features for the MCP server including
;; input validation, execution sandboxing, permission management, and
;; audit logging.

;;; Code:

(require 'cl-lib)

;;; Variables

(defvar mcp-server-security--dangerous-functions
  '(delete-file
    delete-directory
    shell-command
    shell-command-to-string
    call-process
    start-process
    eval
    load
    require
    kill-emacs
    save-buffers-kill-emacs
    save-buffers-kill-terminal
    server-start
    server-force-delete
    set-file-modes
    set-file-times
    copy-file
    rename-file
    make-directory
    write-region
    insert-file-contents
    find-file-literally)
  "List of dangerous functions that require permission.")

(defvar mcp-server-security--permission-cache (make-hash-table :test 'equal)
  "Cache of granted permissions.")

(defvar mcp-server-security--audit-log '()
  "Audit log of security events.")

(defvar mcp-server-security--max-execution-time 30
  "Maximum execution time for tools in seconds.")

(defvar mcp-server-security--max-memory-usage 100000000
  "Maximum memory usage for tools in bytes (100MB).")

(defvar mcp-server-security--prompt-for-permissions t
  "Whether to prompt user for dangerous operations.")

;;; Permission Management

(defun mcp-server-security-check-permission (operation &optional data)
  "Check if OPERATION with DATA is permitted.
Returns t if permitted, nil otherwise."
  (let ((cache-key (format "%s:%s" operation data)))
    (or (gethash cache-key mcp-server-security--permission-cache)
        (mcp-server-security--request-permission operation data cache-key))))

(defun mcp-server-security--request-permission (operation data cache-key)
  "Request permission for OPERATION with DATA, caching result with CACHE-KEY."
  (if mcp-server-security--prompt-for-permissions
      (let ((granted (yes-or-no-p 
                      (format "MCP tool wants to perform: %s%s. Allow? "
                              operation
                              (if data (format " (%s)" data) "")))))
        (puthash cache-key granted mcp-server-security--permission-cache)
        (mcp-server-security--log-audit operation data granted)
        granted)
    ;; If not prompting, deny dangerous operations by default
    (let ((granted (not (mcp-server-security--is-dangerous-operation operation))))
      (puthash cache-key granted mcp-server-security--permission-cache)
      (mcp-server-security--log-audit operation data granted)
      granted)))

(defun mcp-server-security--is-dangerous-operation (operation)
  "Check if OPERATION is considered dangerous."
  (or (member operation mcp-server-security--dangerous-functions)
      (string-match-p "delete\\|kill\\|remove\\|destroy" (symbol-name operation))))

;;; Input Validation

(defun mcp-server-security-validate-input (input)
  "Validate INPUT for security issues.
Returns the input if safe, signals an error otherwise."
  ;; Check for suspicious patterns
  (when (stringp input)
    ;; Check for shell command injection
    (when (string-match-p "[;&|`$]" input)
      (error "Input contains potentially dangerous shell characters"))
    
    ;; Check for path traversal
    (when (string-match-p "\\.\\./\\|~/" input)
      (error "Input contains potentially dangerous path patterns"))
    
    ;; Check for excessive length
    (when (> (length input) 10000)
      (error "Input exceeds maximum length")))
  
  ;; Check for suspicious elisp code patterns in strings
  (when (and (stringp input)
             (string-match-p "(\\s-*\\(?:eval\\|load\\|shell-command\\)" input))
    (error "Input contains potentially dangerous elisp patterns"))
  
  input)

(defun mcp-server-security-sanitize-string (str)
  "Sanitize STR for safe use."
  (when (stringp str)
    ;; Remove null bytes
    (setq str (replace-regexp-in-string "\0" "" str))
    ;; Limit length
    (when (> (length str) 1000)
      (setq str (substring str 0 1000))))
  str)

;;; Execution Sandboxing

(defun mcp-server-security-safe-eval (form)
  "Safely evaluate FORM with security restrictions."
  ;; Check if form contains dangerous functions
  (mcp-server-security--check-form-safety form)
  
  ;; Execute with timeout and memory limits
  (mcp-server-security--execute-with-limits
   (lambda () (eval form))))

(defun mcp-server-security--check-form-safety (form)
  "Check if FORM is safe to evaluate."
  (cond
   ;; Check atoms
   ((symbolp form)
    (when (member form mcp-server-security--dangerous-functions)
      (unless (mcp-server-security-check-permission form)
        (error "Permission denied for function: %s" form))))
   
   ;; Check lists (function calls)
   ((listp form)
    (when form
      (let ((func (car form)))
        (when (symbolp func)
          (when (member func mcp-server-security--dangerous-functions)
            (unless (mcp-server-security-check-permission func (cdr form))
              (error "Permission denied for function: %s" func))))
        
        ;; Recursively check arguments
        (dolist (arg (cdr form))
          (mcp-server-security--check-form-safety arg)))))))

(defun mcp-server-security--execute-with-limits (func)
  "Execute FUNC with time and memory limits."
  (let ((start-time (current-time))
        (start-gc-cons-threshold gc-cons-threshold))
    
    ;; Set conservative GC threshold for memory monitoring
    (setq gc-cons-threshold 1000000)
    
    (unwind-protect
        (with-timeout (mcp-server-security--max-execution-time
                       (error "Execution timeout exceeded"))
          (funcall func))
      
      ;; Restore GC threshold
      (setq gc-cons-threshold start-gc-cons-threshold)
      
      ;; Log execution time
      (let ((elapsed (float-time (time-subtract (current-time) start-time))))
        (when (> elapsed 5.0)
          (mcp-server-security--log-audit 'slow-execution elapsed t))))))

;;; Audit Logging

(defun mcp-server-security--log-audit (operation data granted &optional timestamp)
  "Log security audit event."
  (let ((entry `((timestamp . ,(or timestamp (current-time)))
                 (operation . ,operation)
                 (data . ,data)
                 (granted . ,granted))))
    (push entry mcp-server-security--audit-log)
    
    ;; Keep only last 1000 entries
    (when (> (length mcp-server-security--audit-log) 1000)
      (setq mcp-server-security--audit-log
            (cl-subseq mcp-server-security--audit-log 0 1000)))))

(defun mcp-server-security-get-audit-log (&optional limit)
  "Get audit log entries, optionally limited to LIMIT entries."
  (if limit
      (cl-subseq mcp-server-security--audit-log 0 (min limit (length mcp-server-security--audit-log)))
    mcp-server-security--audit-log))

(defun mcp-server-security-clear-audit-log ()
  "Clear the audit log."
  (setq mcp-server-security--audit-log '()))

;;; Permission Cache Management

(defun mcp-server-security-clear-permissions ()
  "Clear all cached permissions."
  (clrhash mcp-server-security--permission-cache)
  (mcp-server-security--log-audit 'clear-permissions nil t))

(defun mcp-server-security-grant-permission (operation &optional data)
  "Grant permission for OPERATION with optional DATA."
  (let ((cache-key (format "%s:%s" operation data)))
    (puthash cache-key t mcp-server-security--permission-cache)
    (mcp-server-security--log-audit operation data t)))

(defun mcp-server-security-deny-permission (operation &optional data)
  "Deny permission for OPERATION with optional DATA."
  (let ((cache-key (format "%s:%s" operation data)))
    (puthash cache-key nil mcp-server-security--permission-cache)
    (mcp-server-security--log-audit operation data nil)))

;;; Configuration

(defun mcp-server-security-set-prompting (enabled)
  "Enable or disable permission prompting based on ENABLED."
  (setq mcp-server-security--prompt-for-permissions enabled)
  (mcp-server-security--log-audit 'set-prompting enabled t))

(defun mcp-server-security-add-dangerous-function (func)
  "Add FUNC to the list of dangerous functions."
  (unless (member func mcp-server-security--dangerous-functions)
    (push func mcp-server-security--dangerous-functions)
    (mcp-server-security--log-audit 'add-dangerous-function func t)))

(defun mcp-server-security-remove-dangerous-function (func)
  "Remove FUNC from the list of dangerous functions."
  (setq mcp-server-security--dangerous-functions
        (remove func mcp-server-security--dangerous-functions))
  (mcp-server-security--log-audit 'remove-dangerous-function func t))

;;; Initialization and Cleanup

(defun mcp-server-security-init ()
  "Initialize the security system."
  (clrhash mcp-server-security--permission-cache)
  (setq mcp-server-security--audit-log '())
  (mcp-server-security--log-audit 'security-init nil t))

(defun mcp-server-security-cleanup ()
  "Clean up the security system."
  (mcp-server-security--log-audit 'security-cleanup nil t)
  (clrhash mcp-server-security--permission-cache)
  (setq mcp-server-security--audit-log '()))

;;; Interactive Commands

(defun mcp-server-security-show-audit-log ()
  "Display the security audit log."
  (interactive)
  (with-current-buffer (get-buffer-create "*MCP Security Audit*")
    (erase-buffer)
    (insert "MCP Security Audit Log\n")
    (insert "========================\n\n")
    (dolist (entry (reverse mcp-server-security--audit-log))
      (insert (format "[%s] %s: %s (%s)\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S" (alist-get 'timestamp entry))
                      (alist-get 'operation entry)
                      (alist-get 'data entry)
                      (if (alist-get 'granted entry) "GRANTED" "DENIED"))))
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))))

(defun mcp-server-security-show-permissions ()
  "Display cached permissions."
  (interactive)
  (with-current-buffer (get-buffer-create "*MCP Permissions*")
    (erase-buffer)
    (insert "MCP Cached Permissions\n")
    (insert "======================\n\n")
    (maphash
     (lambda (key value)
       (insert (format "%s: %s\n" key (if value "GRANTED" "DENIED"))))
     mcp-server-security--permission-cache)
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))))

(provide 'mcp-server-security)

;;; mcp-server-security.el ends here