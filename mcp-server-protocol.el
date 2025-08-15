;;; mcp-server-protocol.el --- MCP Protocol Implementation -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This module implements the core MCP (Model Context Protocol) handling
;; including JSON-RPC 2.0 message parsing, stdio transport, and protocol
;; lifecycle management.

;;; Code:

(require 'json)

;;; Variables

(defvar mcp-server-protocol--message-handler nil
  "Function to handle incoming MCP messages.")

(defvar mcp-server-protocol--input-buffer ""
  "Buffer for accumulating input from stdin.")

(defvar mcp-server-protocol--running nil
  "Whether the protocol handler is running.")

;;; JSON-RPC Message Handling

(defun mcp-server-protocol--parse-message (line)
  "Parse a JSON-RPC message from LINE."
  (condition-case err
      (json-parse-string line :object-type 'alist :array-type 'vector)
    (json-error
     (error "Invalid JSON in message: %s" (error-message-string err)))))

(defun mcp-server-protocol--format-message (message)
  "Format MESSAGE as JSON-RPC string."
  (json-encode message))

(defun mcp-server-protocol--validate-message (message)
  "Validate that MESSAGE is a proper JSON-RPC 2.0 message."
  (unless (alist-get 'jsonrpc message)
    (error "Missing jsonrpc field"))
  
  (unless (string= (alist-get 'jsonrpc message) "2.0")
    (error "Invalid jsonrpc version: %s" (alist-get 'jsonrpc message)))
  
  ;; Check if it's a request (has method and id)
  (when (and (alist-get 'method message)
             (not (alist-get 'id message)))
    ;; It's a notification (method but no id) - that's valid
    nil)
  
  ;; Check if it's a response (has id but no method)
  (when (and (alist-get 'id message)
             (not (alist-get 'method message)))
    ;; Must have either result or error
    (unless (or (alist-get 'result message)
                (alist-get 'error message))
      (error "Response must have either result or error")))
  
  message)

;;; stdio Transport

(defun mcp-server-protocol--read-stdin ()
  "Read and process messages from stdin."
  (let ((input (read-from-minibuffer "")))
    (when input
      (setq mcp-server-protocol--input-buffer 
            (concat mcp-server-protocol--input-buffer input))
      
      ;; Process complete lines (messages)
      (while (string-match "\n" mcp-server-protocol--input-buffer)
        (let* ((line-end (match-end 0))
               (line (substring mcp-server-protocol--input-buffer 0 (1- line-end))))
          
          ;; Remove processed line from buffer
          (setq mcp-server-protocol--input-buffer 
                (substring mcp-server-protocol--input-buffer line-end))
          
          ;; Process the line if it's not empty
          (when (> (length (string-trim line)) 0)
            (mcp-server-protocol--process-line line)))))))

(defun mcp-server-protocol--process-line (line)
  "Process a single LINE of input."
  (condition-case err
      (let ((message (mcp-server-protocol--parse-message line)))
        (mcp-server-protocol--validate-message message)
        (when mcp-server-protocol--message-handler
          (funcall mcp-server-protocol--message-handler message)))
    (error
     (princ (mcp-server-protocol--format-message
             `((jsonrpc . "2.0")
               (id . null)
               (error . ((code . -32700)
                         (message . "Parse error")
                         (data . ,(error-message-string err)))))))
     (princ "\n")
     (flush-standard-output))))

(defun mcp-server-protocol--write-stdout (message)
  "Write MESSAGE to stdout."
  (let ((json-str (mcp-server-protocol--format-message message)))
    (princ json-str)
    (princ "\n")
    (flush-standard-output)))

;;; Message Sending

(defun mcp-server-protocol-send-response (id result)
  "Send a successful response with ID and RESULT."
  (mcp-server-protocol--write-stdout
   `((jsonrpc . "2.0")
     (id . ,id)
     (result . ,result))))

(defun mcp-server-protocol-send-error (id code message &optional data)
  "Send an error response with ID, CODE, MESSAGE and optional DATA."
  (mcp-server-protocol--write-stdout
   `((jsonrpc . "2.0")
     (id . ,id)
     (error . ((code . ,code)
               (message . ,message)
               ,@(when data `((data . ,data))))))))

(defun mcp-server-protocol-send-notification (method &optional params)
  "Send a notification with METHOD and optional PARAMS."
  (mcp-server-protocol--write-stdout
   `((jsonrpc . "2.0")
     (method . ,method)
     ,@(when params `((params . ,params))))))

;;; Protocol Lifecycle

(defun mcp-server-protocol-start (message-handler)
  "Start the protocol handler with MESSAGE-HANDLER function."
  (when mcp-server-protocol--running
    (error "Protocol handler is already running"))
  
  (setq mcp-server-protocol--message-handler message-handler)
  (setq mcp-server-protocol--input-buffer "")
  (setq mcp-server-protocol--running t)
  
  ;; In a real implementation, we would set up stdin reading here
  ;; For now, we'll implement a polling mechanism or process filter
  ;; This is a simplified version for the initial implementation
  )

(defun mcp-server-protocol-stop ()
  "Stop the protocol handler."
  (setq mcp-server-protocol--running nil)
  (setq mcp-server-protocol--message-handler nil)
  (setq mcp-server-protocol--input-buffer ""))

;;; Input Processing (Simplified)

(defun mcp-server-protocol-process-input (input)
  "Process INPUT string containing JSON-RPC messages.
This is a simplified version for testing - in production we'd use stdin."
  (setq mcp-server-protocol--input-buffer 
        (concat mcp-server-protocol--input-buffer input))
  
  ;; Process complete lines
  (while (string-match "\n" mcp-server-protocol--input-buffer)
    (let* ((line-end (match-end 0))
           (line (substring mcp-server-protocol--input-buffer 0 (1- line-end))))
      
      ;; Remove processed line from buffer
      (setq mcp-server-protocol--input-buffer 
            (substring mcp-server-protocol--input-buffer line-end))
      
      ;; Process the line if it's not empty
      (when (> (length (string-trim line)) 0)
        (mcp-server-protocol--process-line line)))))

;;; Utility Functions

(defun mcp-server-protocol-running-p ()
  "Return t if the protocol handler is running."
  mcp-server-protocol--running)

(defun mcp-server-protocol-clear-buffer ()
  "Clear the input buffer."
  (setq mcp-server-protocol--input-buffer ""))

(provide 'mcp-server-protocol)

;;; mcp-server-protocol.el ends here