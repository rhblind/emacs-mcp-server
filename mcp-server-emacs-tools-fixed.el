;;; mcp-server-emacs-tools.el --- Emacs-specific MCP Tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This module provides Emacs-specific MCP tools that expose Emacs
;; functionality like buffer operations, cursor management, variable
;; access, and command execution.

;;; Code:

(require 'mcp-server-tools)
(require 'mcp-server-security)
(require 'cl-lib)

;;; Core Elisp Tools

(defun mcp-server-emacs-tools-register ()
  "Register all Emacs-specific MCP tools."
  
  ;; eval-elisp - Execute arbitrary elisp expressions
  (mcp-server-tools-register
   "eval-elisp"
   "Execute Elisp Expression"
   "Execute arbitrary Elisp code and return the result. Use with caution as this provides full access to Emacs functionality."
   '((type . "object")
     (properties . ((expression . ((type . "string")
                                   (description . "The Elisp expression to evaluate")))))
     (required . ("expression")))
   #'mcp-server-emacs-tools--eval-elisp)
  
  ;; get-buffer-content - Read buffer contents
  (mcp-server-tools-register
   "get-buffer-content"
   "Get Buffer Content"
   "Get the content of a buffer by name or the current buffer if no name is provided."
   '((type . "object")
     (properties . ((buffer . ((type . "string")
                               (description . "Name of the buffer (optional, defaults to current buffer)")))))
   #'mcp-server-emacs-tools--get-buffer-content)
  
  ;; set-buffer-content - Modify buffer contents
  (mcp-server-tools-register
   "set-buffer-content"
   "Set Buffer Content"
   "Replace the entire content of a buffer."
   '((type . "object")
     (properties . ((buffer . ((type . "string")
                               (description . "Name of the buffer (optional, defaults to current buffer)")))
                    (content . ((type . "string")
                               (description . "New content for the buffer")))))
     (required . ("content")))
   #'mcp-server-emacs-tools--set-buffer-content)
  
  ;; get-buffer-list - List all buffers
  (mcp-server-tools-register
   "get-buffer-list"
   "Get Buffer List"
   "Get a list of all open buffers with their names and major modes."
   '((type . "object")
     (properties . ()))
   #'mcp-server-emacs-tools--get-buffer-list)
  
  ;; switch-buffer - Change active buffer
  (mcp-server-tools-register
   "switch-buffer"
   "Switch Buffer"
   "Switch to a different buffer by name."
   '((type . "object")
     (properties . ((buffer . ((type . "string")
                               (description . "Name of the buffer to switch to")))))
     (required . ("buffer")))
   #'mcp-server-emacs-tools--switch-buffer)
  
  ;; get-point - Get cursor position
  (mcp-server-tools-register
   "get-point"
   "Get Cursor Position"
   "Get the current cursor position (point) in the buffer."
   '((type . "object")
     (properties . ((buffer . ((type . "string")
                               (description . "Name of the buffer (optional, defaults to current buffer)")))))
   #'mcp-server-emacs-tools--get-point)
  
  ;; goto-point - Move cursor to position
  (mcp-server-tools-register
   "goto-point"
   "Go to Position"
   "Move the cursor to a specific position in the buffer."
   '((type . "object")
     (properties . ((position . ((type . "number")
                                 (description . "The position to move to")))
                    (buffer . ((type . "string")
                              (description . "Name of the buffer (optional, defaults to current buffer)")))))
     (required . ("position")))
   #'mcp-server-emacs-tools--goto-point)
  
  ;; insert-at-point - Insert text at cursor
  (mcp-server-tools-register
   "insert-at-point"
   "Insert Text at Cursor"
   "Insert text at the current cursor position."
   '((type . "object")
     (properties . ((text . ((type . "string")
                             (description . "Text to insert")))
                    (buffer . ((type . "string")
                              (description . "Name of the buffer (optional, defaults to current buffer)")))))
     (required . ("text")))
   #'mcp-server-emacs-tools--insert-at-point)
  
  ;; get-selection - Get selected text
  (mcp-server-tools-register
   "get-selection"
   "Get Selected Text"
   "Get the currently selected text in the buffer."
   '((type . "object")
     (properties . ((buffer . ((type . "string")
                               (description . "Name of the buffer (optional, defaults to current buffer)")))))
   #'mcp-server-emacs-tools--get-selection)
  
  ;; get-variable - Read Emacs variable
  (mcp-server-tools-register
   "get-variable"
   "Get Variable Value"
   "Get the value of an Emacs variable."
   '((type . "object")
     (properties . ((variable . ((type . "string")
                                 (description . "Name of the variable")))))
     (required . ("variable")))
   #'mcp-server-emacs-tools--get-variable)
  
  ;; set-variable - Set Emacs variable
  (mcp-server-tools-register
   "set-variable"
   "Set Variable Value"
   "Set the value of an Emacs variable."
   '((type . "object")
     (properties . ((variable . ((type . "string")
                                 (description . "Name of the variable")))
                    (value . ((description . "New value for the variable")))))
     (required . ("variable" "value")))
   #'mcp-server-emacs-tools--set-variable)
  
  ;; call-command - Execute interactive command
  (mcp-server-tools-register
   "call-command"
   "Call Interactive Command"
   "Execute an Emacs interactive command."
   '((type . "object")
     (properties . ((command . ((type . "string")
                                (description . "Name of the command to execute")))
                    (args . ((type . "array")
                            (description . "Arguments to pass to the command (optional)")))))
     (required . ("command")))
   #'mcp-server-emacs-tools--call-command)
  
  ;; get-major-mode - Get buffer's major mode
  (mcp-server-tools-register
   "get-major-mode"
   "Get Major Mode"
   "Get the major mode of a buffer."
   '((type . "object")
     (properties . ((buffer . ((type . "string")
                               (description . "Name of the buffer (optional, defaults to current buffer)")))))
   #'mcp-server-emacs-tools--get-major-mode)
  
  ;; get-window-configuration - Get window layout
  (mcp-server-tools-register
   "get-window-configuration"
   "Get Window Configuration"
   "Get information about the current window configuration."
   '((type . "object")
     (properties . ()))
   #'mcp-server-emacs-tools--get-window-configuration))

;;; Tool Implementation Functions

(defun mcp-server-emacs-tools--eval-elisp (args)
  "Execute elisp expression from ARGS."
  (let ((expression (alist-get 'expression args)))
    (mcp-server-security-validate-input expression)
    
    (condition-case err
        (let ((form (read-from-string expression)))
          (let ((result (mcp-server-security-safe-eval (car form))))
            (format "%S" result)))
      (error
       (format "Error: %s" (error-message-string err))))))

(defun mcp-server-emacs-tools--get-buffer-content (args)
  "Get buffer content from ARGS."
  (let ((buffer-name (alist-get 'buffer args)))
    (with-current-buffer (if buffer-name
                             (get-buffer buffer-name)
                           (current-buffer))
      (if (buffer-live-p (current-buffer))
          (buffer-string)
        (error "Buffer does not exist or is not live")))))

(defun mcp-server-emacs-tools--set-buffer-content (args)
  "Set buffer content from ARGS."
  (let ((buffer-name (alist-get 'buffer args))
        (content (alist-get 'content args)))
    
    ;; Check permission for buffer modification
    (unless (mcp-server-security-check-permission 'modify-buffer buffer-name)
      (error "Permission denied for buffer modification"))
    
    (mcp-server-security-validate-input content)
    
    (with-current-buffer (if buffer-name
                             (get-buffer buffer-name)
                           (current-buffer))
      (if (buffer-live-p (current-buffer))
          (progn
            (erase-buffer)
            (insert content)
            (format "Buffer content updated (%d characters)" (length content)))
        (error "Buffer does not exist or is not live")))))

(defun mcp-server-emacs-tools--get-buffer-list (args)
  "Get list of all buffers from ARGS."
  (let ((buffers '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (push `((name . ,(buffer-name))
                (mode . ,(symbol-name major-mode))
                (file . ,(buffer-file-name))
                (modified . ,(buffer-modified-p))
                (size . ,(buffer-size)))
              buffers)))
    (json-encode (nreverse buffers))))

(defun mcp-server-emacs-tools--switch-buffer (args)
  "Switch to buffer specified in ARGS."
  (let ((buffer-name (alist-get 'buffer args)))
    (if (get-buffer buffer-name)
        (progn
          (switch-to-buffer buffer-name)
          (format "Switched to buffer: %s" buffer-name))
      (error "Buffer does not exist: %s" buffer-name))))

(defun mcp-server-emacs-tools--get-point (args)
  "Get cursor position from ARGS."
  (let ((buffer-name (alist-get 'buffer args)))
    (with-current-buffer (if buffer-name
                             (get-buffer buffer-name)
                           (current-buffer))
      (if (buffer-live-p (current-buffer))
          `((point . ,(point))
            (line . ,(line-number-at-pos))
            (column . ,(current-column))
            (buffer . ,(buffer-name)))
        (error "Buffer does not exist or is not live")))))

(defun mcp-server-emacs-tools--goto-point (args)
  "Move cursor to position specified in ARGS."
  (let ((position (alist-get 'position args))
        (buffer-name (alist-get 'buffer args)))
    
    (with-current-buffer (if buffer-name
                             (get-buffer buffer-name)
                           (current-buffer))
      (if (buffer-live-p (current-buffer))
          (progn
            (goto-char position)
            (format "Moved to position %d (line %d, column %d)"
                    (point)
                    (line-number-at-pos)
                    (current-column)))
        (error "Buffer does not exist or is not live")))))

(defun mcp-server-emacs-tools--insert-at-point (args)
  "Insert text at cursor position from ARGS."
  (let ((text (alist-get 'text args))
        (buffer-name (alist-get 'buffer args)))
    
    ;; Check permission for buffer modification
    (unless (mcp-server-security-check-permission 'modify-buffer buffer-name)
      (error "Permission denied for buffer modification"))
    
    (mcp-server-security-validate-input text)
    
    (with-current-buffer (if buffer-name
                             (get-buffer buffer-name)
                           (current-buffer))
      (if (buffer-live-p (current-buffer))
          (progn
            (insert text)
            (format "Inserted %d characters at position %d" (length text) (point)))
        (error "Buffer does not exist or is not live")))))

(defun mcp-server-emacs-tools--get-selection (args)
  "Get selected text from ARGS."
  (let ((buffer-name (alist-get 'buffer args)))
    (with-current-buffer (if buffer-name
                             (get-buffer buffer-name)
                           (current-buffer))
      (if (buffer-live-p (current-buffer))
          (if (use-region-p)
              `((text . ,(buffer-substring-no-properties (region-beginning) (region-end)))
                (start . ,(region-beginning))
                (end . ,(region-end))
                (active . t))
            `((text . "")
              (start . nil)
              (end . nil)
              (active . nil)))
        (error "Buffer does not exist or is not live")))))

(defun mcp-server-emacs-tools--get-variable (args)
  "Get variable value from ARGS."
  (let ((variable-name (alist-get 'variable args)))
    (mcp-server-security-validate-input variable-name)
    
    (let ((var-symbol (intern variable-name)))
      (if (boundp var-symbol)
          `((variable . ,variable-name)
            (value . ,(symbol-value var-symbol))
            (type . ,(type-of (symbol-value var-symbol))))
        (error "Variable is not bound: %s" variable-name)))))

(defun mcp-server-emacs-tools--set-variable (args)
  "Set variable value from ARGS."
  (let ((variable-name (alist-get 'variable args))
        (value (alist-get 'value args)))
    
    ;; Check permission for variable modification
    (unless (mcp-server-security-check-permission 'set-variable variable-name)
      (error "Permission denied for variable modification"))
    
    (mcp-server-security-validate-input variable-name)
    
    (let ((var-symbol (intern variable-name)))
      (set var-symbol value)
      (format "Variable %s set to: %S" variable-name value))))

(defun mcp-server-emacs-tools--call-command (args)
  "Call interactive command from ARGS."
  (let ((command-name (alist-get 'command args))
        (command-args (alist-get 'args args)))
    
    ;; Check permission for command execution
    (unless (mcp-server-security-check-permission 'call-command command-name)
      (error "Permission denied for command execution"))
    
    (mcp-server-security-validate-input command-name)
    
    (let ((command-symbol (intern command-name)))
      (if (commandp command-symbol)
          (condition-case err
              (progn
                (if command-args
                    (apply command-symbol command-args)
                  (call-interactively command-symbol))
                (format "Command executed: %s" command-name))
            (error
             (format "Command failed: %s" (error-message-string err))))
        (error "Not a valid command: %s" command-name)))))

(defun mcp-server-emacs-tools--get-major-mode (args)
  "Get major mode from ARGS."
  (let ((buffer-name (alist-get 'buffer args)))
    (with-current-buffer (if buffer-name
                             (get-buffer buffer-name)
                           (current-buffer))
      (if (buffer-live-p (current-buffer))
          `((buffer . ,(buffer-name))
            (major-mode . ,(symbol-name major-mode))
            (minor-modes . ,(mapcar #'symbol-name 
                                   (cl-loop for mode in minor-mode-list
                                           when (and (boundp mode) (symbol-value mode))
                                           collect mode))))
        (error "Buffer does not exist or is not live")))))

(defun mcp-server-emacs-tools--get-window-configuration (args)
  "Get window configuration from ARGS."
  (let ((windows '()))
    (walk-windows
     (lambda (window)
       (let ((buffer (window-buffer window)))
         (push `((buffer . ,(buffer-name buffer))
                 (start . ,(window-start window))
                 (point . ,(window-point window))
                 (height . ,(window-height window))
                 (width . ,(window-width window))
                 (left . ,(nth 0 (window-edges window)))
                 (top . ,(nth 1 (window-edges window))))
               windows)))
     nil t)
    `((windows . ,(nreverse windows))
      (selected-window . ,(buffer-name (window-buffer (selected-window))))
      (frame-count . ,(length (frame-list))))))

(provide 'mcp-server-emacs-tools-fixed)

;;; mcp-server-emacs-tools-fixed.el ends here