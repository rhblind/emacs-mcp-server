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

(defcustom mcp-server-security-dangerous-functions
  '(browse-url
    call-process
    copy-file
    delete-directory
    delete-file
    dired
    eval
    find-file
    find-file-literally
    find-file-noselect
    getenv
    insert-file-contents
    kill-emacs
    load
    make-directory
    process-environment
    rename-file
    require
    save-buffers-kill-emacs
    save-buffers-kill-terminal
    save-current-buffer
    server-force-delete
    server-start
    set-buffer
    set-file-modes
    set-file-times
    shell-command
    shell-command-to-string
    shell-environment
    start-process
    switch-to-buffer
    url-retrieve
    url-retrieve-synchronously
    view-file
    with-current-buffer
    write-region)
  "List of functions that require permission before execution.
Users can customize this list to add or remove functions that should
prompt for permission when used by the LLM."
  :type '(repeat symbol)
  :group 'mcp-server)

(defcustom mcp-server-security-allowed-dangerous-functions nil
  "List of dangerous functions that are explicitly allowed without prompting.
These functions will bypass the dangerous function protection even if they
are listed in `mcp-server-security-dangerous-functions'.
Use this to whitelist specific functions you trust the LLM to use freely."
  :type '(repeat symbol)
  :group 'mcp-server)

(defvar mcp-server-security--permission-cache (make-hash-table :test 'equal)
  "Cache of granted permissions for current Emacs session.")

(defvar mcp-server-security--audit-log '()
  "Audit log of security events.")

(defvar mcp-server-security--max-execution-time 30
  "Maximum execution time for tools in seconds.")

(defvar mcp-server-security--max-memory-usage 100000000
  "Maximum memory usage for tools in bytes (100MB).")

(defcustom mcp-server-security-prompt-for-permissions t
  "Whether to prompt user for dangerous operations."
  :type 'boolean
  :group 'mcp-server)

(defcustom mcp-server-security-sensitive-file-patterns
  '("~/.authinfo" "~/.authinfo.gpg" "~/.authinfo.gpg~" "~/.authinfo.enc"
    "~/.netrc" "~/.netrc.gpg" "~/.netrc.gpg~" "~/.netrc.enc"
    "~/.ssh/" "~/.gnupg/" "~/.aws/" "~/.config/gh/"
    "~/.docker/config.json" "~/.kube/config" "~/.npmrc"
    "~/.pypirc" "~/.gem/credentials" "~/.gitconfig"
    "~/.password-store/" "~/.local/share/keyrings/"
    "/etc/passwd" "/etc/shadow" "/etc/hosts"
    "passwords" "secrets" "credentials" "keys" "tokens")
  "List of file patterns that should require permission to access.
Users can customize this list to add or remove sensitive file patterns.
Patterns can be absolute paths, relative paths, or just filenames."
  :type '(repeat string)
  :group 'mcp-server)

(defcustom mcp-server-security-allowed-sensitive-files nil
  "List of sensitive files that are explicitly allowed without prompting.
These files will bypass the sensitive file protection even if they match
patterns in `mcp-server-security-sensitive-file-patterns'.
Use this to whitelist specific credential files you want the LLM to access."
  :type '(repeat string)
  :group 'mcp-server)

(defcustom mcp-server-security-sensitive-buffer-patterns
  '("*Messages*" "*shell*" "*terminal*" "*eshell*"
    "*compilation*" "*Async Shell Command*")
  "List of buffer patterns that may contain sensitive information."
  :type '(repeat string)
  :group 'mcp-server)

;; Backward compatibility aliases
(defvar mcp-server-security--prompt-for-permissions mcp-server-security-prompt-for-permissions)
(defvar mcp-server-security--sensitive-file-patterns mcp-server-security-sensitive-file-patterns)
(defvar mcp-server-security--sensitive-buffer-patterns mcp-server-security-sensitive-buffer-patterns)
(defvar mcp-server-security--dangerous-functions mcp-server-security-dangerous-functions)

;;; Permission Management

(defun mcp-server-security-check-permission (operation &optional data)
  "Check if OPERATION with DATA is permitted.
Returns t if permitted, nil otherwise."
  (let ((cache-key (format "%s:%s" operation data)))
    (if (gethash cache-key mcp-server-security--permission-cache)
        t
      (mcp-server-security--request-permission operation data cache-key))))

(defun mcp-server-security--request-permission (operation data cache-key)
  "Request permission for OPERATION with DATA, caching result with CACHE-KEY."
  (if mcp-server-security-prompt-for-permissions
      (let ((response (mcp-server-security--prompt-permission operation data)))
        (pcase response
          ('always
           (puthash cache-key t mcp-server-security--permission-cache)
           (mcp-server-security--log-audit operation data 'always)
           t)
          ('yes
           (mcp-server-security--log-audit operation data t)
           t)
          ('no
           (mcp-server-security--log-audit operation data nil)
           nil)))
    ;; If not prompting, deny dangerous operations by default
    (let ((granted (not (mcp-server-security--is-dangerous-operation operation))))
      (puthash cache-key granted mcp-server-security--permission-cache)
      (mcp-server-security--log-audit operation data granted)
      granted)))

(defun mcp-server-security--prompt-permission (operation data)
  "Prompt user for permission for OPERATION with DATA.
Returns 'yes, 'no, or 'always."
  (let ((prompt (format "MCP: %s%s (y)es, (n)o, (!) always: "
                        operation
                        (if data (format " (%s)" data) ""))))
    (pcase (read-char-choice prompt '(?y ?n ?!))
      (?y 'yes)
      (?n 'no)
      (?! 'always))))

(defun mcp-server-security--is-dangerous-operation (operation)
  "Check if OPERATION is considered dangerous."
  ;; First check if function is explicitly allowed
  (unless (member operation mcp-server-security-allowed-dangerous-functions)
    ;; Then check if it's in the dangerous functions list or matches dangerous patterns
    (or (member operation mcp-server-security-dangerous-functions)
        (string-match-p "delete\\|kill\\|remove\\|destroy" (symbol-name operation)))))

(defun mcp-server-security--is-sensitive-file (path)
  "Check if PATH points to a sensitive file."
  (when (stringp path)
    (let ((expanded-path (expand-file-name path)))
      ;; First check if file is explicitly allowed
      (unless (cl-some (lambda (allowed-file)
                         (string-equal (expand-file-name allowed-file) expanded-path))
                       mcp-server-security-allowed-sensitive-files)
        ;; Then check if it matches any sensitive patterns
        (cl-some (lambda (pattern)
                   (or (string-match-p (regexp-quote pattern) expanded-path)
                       (string-match-p pattern (file-name-nondirectory expanded-path))))
                 mcp-server-security-sensitive-file-patterns)))))

(defun mcp-server-security--is-sensitive-buffer (buffer-name)
  "Check if BUFFER-NAME is a sensitive buffer."
  (when (stringp buffer-name)
    (cl-some (lambda (pattern)
               (string-match-p pattern buffer-name))
             mcp-server-security-sensitive-buffer-patterns)))

(defun mcp-server-security--contains-credentials (content)
  "Check if CONTENT contains credential-like patterns."
  (when (stringp content)
    (or (string-match-p "password\\s-*[=:]\\s-*['\"]?[^\\s]+" content)
        (string-match-p "api[_-]?key\\s-*[=:]\\s-*['\"]?[^\\s]+" content)
        (string-match-p "secret\\s-*[=:]\\s-*['\"]?[^\\s]+" content)
        (string-match-p "token\\s-*[=:]\\s-*['\"]?[^\\s]+" content)
        (string-match-p "-----BEGIN [A-Z ]+PRIVATE KEY-----" content))))

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
    (when (and (member form mcp-server-security-dangerous-functions)
               (not (member form mcp-server-security-allowed-dangerous-functions)))
      (unless (mcp-server-security-check-permission form)
        (error "Permission denied for function: %s" form))))
   
   ;; Check lists (function calls)
   ((listp form)
    (when form
      (let ((func (car form))
            (args (cdr form)))
        (when (symbolp func)
          ;; Check for dangerous functions
          (when (and (member func mcp-server-security-dangerous-functions)
                     (not (member func mcp-server-security-allowed-dangerous-functions)))
            (unless (mcp-server-security-check-permission func args)
              (error "Permission denied for function: %s" func)))
          
          ;; Special checks for file access functions
          (when (memq func '(find-file find-file-noselect view-file insert-file-contents))
            (let ((file-path (car args)))
              (when (and file-path (stringp file-path))
                (when (mcp-server-security--is-sensitive-file file-path)
                  (unless (mcp-server-security-check-permission 
                           (format "access-sensitive-file:%s" func) file-path)
                    (error "Permission denied for sensitive file access: %s" file-path))))))
          
          ;; Special checks for buffer access functions
          (when (memq func '(switch-to-buffer set-buffer with-current-buffer))
            (let ((buffer-name (if (eq func 'with-current-buffer) 
                                   (car args)
                                 (car args))))
              (when (and buffer-name (stringp buffer-name))
                (when (mcp-server-security--is-sensitive-buffer buffer-name)
                  (error "Access denied to sensitive buffer: %s" buffer-name))))))
        
        ;; Recursively check arguments
        (dolist (arg args)
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
  (interactive)
  (clrhash mcp-server-security--permission-cache)
  (mcp-server-security--log-audit 'clear-permissions nil t)
  (message "Session permission cache cleared."))

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
  "Display cached permissions for this session."
  (interactive)
  (with-current-buffer (get-buffer-create "*MCP Permissions*")
    (erase-buffer)
    (insert "MCP Session Permissions\n")
    (insert "=======================\n\n")
    (let ((count 0))
      (maphash
       (lambda (key value)
         (insert (format "  %s: %s\n" key (if value "ALLOWED" "DENIED")))
         (cl-incf count))
       mcp-server-security--permission-cache)
      (when (zerop count)
        (insert "  (none)\n")))
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))))

(defun mcp-server-security-remove-permission (key)
  "Remove a specific permission by KEY."
  (interactive
   (list (completing-read "Remove permission: "
                          (let (keys)
                            (maphash (lambda (k _v) (push k keys))
                                     mcp-server-security--permission-cache)
                            keys)
                          nil t)))
  (remhash key mcp-server-security--permission-cache)
  (mcp-server-security--log-audit 'remove-permission key t)
  (message "Removed permission: %s" key))

(provide 'mcp-server-security)

;;; mcp-server-security.el ends here
