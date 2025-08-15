;;; mcp-server-transport.el --- Transport Interface for MCP Server -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This module provides an abstract transport interface for the MCP server,
;; allowing pluggable transport backends (Unix sockets, TCP, stdio, etc.)

;;; Code:

(require 'cl-lib)

;;; Transport Interface

(cl-defstruct mcp-server-transport
  "Abstract transport interface for MCP server."
  name                    ; Human-readable transport name
  start-fn               ; Function to start the transport: (lambda (message-handler &rest args) ...)
  stop-fn                ; Function to stop the transport: (lambda () ...)
  send-fn                ; Function to send message: (lambda (client-id message) ...)
  status-fn              ; Function to get status: (lambda () ...) -> alist
  list-clients-fn        ; Function to list clients: (lambda () ...) -> list
  disconnect-client-fn)  ; Function to disconnect client: (lambda (client-id) ...)

;;; Transport Registry

(defvar mcp-server-transport--registry (make-hash-table :test 'equal)
  "Registry of available transport implementations.")

(defun mcp-server-transport-register (name transport)
  "Register a transport implementation with NAME."
  (unless (mcp-server-transport-p transport)
    (error "Invalid transport implementation"))
  (puthash name transport mcp-server-transport--registry))

(defun mcp-server-transport-get (name)
  "Get transport implementation by NAME."
  (gethash name mcp-server-transport--registry))

(defun mcp-server-transport-list ()
  "List all registered transport names."
  (hash-table-keys mcp-server-transport--registry))

;;; Transport Operations

(defun mcp-server-transport-start (transport-name message-handler &rest args)
  "Start a transport by NAME with MESSAGE-HANDLER and ARGS."
  (let ((transport (mcp-server-transport-get transport-name)))
    (unless transport
      (error "Transport not found: %s" transport-name))
    (apply (mcp-server-transport-start-fn transport) message-handler args)))

(defun mcp-server-transport-stop (transport-name)
  "Stop a transport by NAME."
  (let ((transport (mcp-server-transport-get transport-name)))
    (when transport
      (funcall (mcp-server-transport-stop-fn transport)))))

(defun mcp-server-transport-send (transport-name client-id message)
  "Send MESSAGE to CLIENT-ID using transport NAME."
  (message "MCP Debug: transport-send START - transport=%s, client=%s" 
           transport-name client-id)
  (let ((transport (mcp-server-transport-get transport-name)))
    (when (and (boundp 'mcp-server-debug) mcp-server-debug)
      (message "MCP Debug: transport object = %S" transport))
    (if transport
        (condition-case err
            (funcall (mcp-server-transport-send-fn transport) client-id message)
          (error
           (message "MCP Debug: Error in transport funcall: %s" (error-message-string err))
           (signal (car err) (cdr err))))
      (when (and (boundp 'mcp-server-debug) mcp-server-debug)
        (message "MCP Debug: No transport found for name: %s" transport-name)))))

(defun mcp-server-transport-send-raw (transport-name client-id json-string)
  "Send raw JSON-STRING to CLIENT-ID using transport NAME.
This bypasses the normal JSON serialization."
  (if (string= transport-name "unix")
      (mcp-server-transport-unix--send-raw client-id json-string)
    (error "Raw send not implemented for transport: %s" transport-name)))

(defun mcp-server-transport-status (transport-name)
  "Get status of transport NAME."
  (let ((transport (mcp-server-transport-get transport-name)))
    (when transport
      (funcall (mcp-server-transport-status-fn transport)))))

(defun mcp-server-transport-list-clients (transport-name)
  "List clients connected to transport NAME."
  (let ((transport (mcp-server-transport-get transport-name)))
    (when transport
      (funcall (mcp-server-transport-list-clients-fn transport)))))

(defun mcp-server-transport-disconnect-client (transport-name client-id)
  "Disconnect CLIENT-ID from transport NAME."
  (let ((transport (mcp-server-transport-get transport-name)))
    (when transport
      (funcall (mcp-server-transport-disconnect-client-fn transport) client-id))))

;;; Message Formatting Utilities

(defun mcp-server-transport--format-json-rpc (message)
  "Format MESSAGE as JSON-RPC string."
  (let ((converted (mcp-server-transport--alist-to-json message)))
    (when (and (boundp 'mcp-server-debug) mcp-server-debug)
      (message "MCP Debug: About to serialize, converted message = %S" converted)
      (when (hash-table-p converted)
        (let ((result-ht (gethash "result" converted)))
          (when (hash-table-p result-ht)
            (message "MCP Debug: isError field value in result: %S (type: %s)" 
                     (gethash "isError" result-ht)
                     (type-of (gethash "isError" result-ht)))))))
    (let ((json-str (json-serialize converted)))
      (when (and (boundp 'mcp-server-debug) mcp-server-debug)
        (message "MCP Debug: Final JSON string: %s" json-str))
      ;; Post-process to ensure proper boolean handling
      ;; Replace any quoted "false" or "true" in isError field with unquoted boolean
      (setq json-str (replace-regexp-in-string 
                      "\"isError\":\"\\(false\\|true\\)\"" 
                      "\"isError\":\\1" 
                      json-str))
      (when (and (boundp 'mcp-server-debug) mcp-server-debug)
        (message "MCP Debug: After post-processing: %s" json-str))
      json-str)))

(defun mcp-server-transport--alist-to-json (obj)
  "Convert alist/list structure OBJ to JSON-serializable format."
  (cond
   ;; If it's nil, return :null for JSON null
   ((null obj)
    :null)
   ;; If it's an alist (list of key-value pairs where each pair is (key . value))
   ;; Check: list of cons cells where each cons has a non-list car (the key)
   ((and (listp obj) 
         (not (null obj)) 
         (seq-every-p (lambda (item) 
                        (and (consp item)
                             (not (listp (car item))))) obj))
    (let ((ht (make-hash-table :test 'equal)))
      (dolist (pair obj)
        (let ((key (if (symbolp (car pair))
                       (symbol-name (car pair))
                     (car pair)))
              (value (mcp-server-transport--alist-to-json (cdr pair))))
          (when (and (boundp 'mcp-server-debug) mcp-server-debug
                     (string= key "isError"))
            (message "MCP Debug: Processing isError - raw value: %S, converted value: %S" 
                     (cdr pair) value))
          (puthash key value ht)))
      ht))
   ;; If it's a list (including array of objects), convert to vector
   ((listp obj)
    (vconcat (mapcar #'mcp-server-transport--alist-to-json obj)))
   ;; If it's a symbol, convert to string (except for special values)
   ((symbolp obj)
    (cond
     ((eq obj t) 
      (when (and (boundp 'mcp-server-debug) mcp-server-debug)
        (message "MCP Debug: Converting symbol t to JSON true"))
      t)
     ((eq obj :null) 
      (when (and (boundp 'mcp-server-debug) mcp-server-debug)
        (message "MCP Debug: Converting symbol :null to JSON null"))
      :null)
     ((eq obj :false) 
      (when (and (boundp 'mcp-server-debug) mcp-server-debug)
        (message "MCP Debug: Converting symbol :false to JSON false"))
      :false)
     (t 
      (when (and (boundp 'mcp-server-debug) mcp-server-debug)
        (message "MCP Debug: Unexpected symbol %S (name: %s, type: %s) being converted to string"
                 obj (symbol-name obj) (type-of obj)))
      (symbol-name obj))))
   ;; Numbers, strings, and other primitives return as-is
   (t obj)))

(defun mcp-server-transport--parse-json-rpc (json-string)
  "Parse JSON-RPC message from JSON-STRING."
  (condition-case err
      (json-parse-string json-string :object-type 'alist :array-type 'list)
    (json-error
     (error "Invalid JSON in message: %s" (error-message-string err)))))

(defun mcp-server-transport--validate-json-rpc (message)
  "Validate that MESSAGE is a proper JSON-RPC 2.0 message."
  (unless (alist-get 'jsonrpc message)
    (error "Missing jsonrpc field"))
  
  (unless (string= (alist-get 'jsonrpc message) "2.0")
    (error "Invalid jsonrpc version: %s" (alist-get 'jsonrpc message)))
  
  message)

;;; Client Management Utilities

(defvar mcp-server-transport--client-counter 0
  "Counter for generating unique client IDs.")

(defun mcp-server-transport--generate-client-id ()
  "Generate a unique client ID."
  (cl-incf mcp-server-transport--client-counter)
  (format "client-%d" mcp-server-transport--client-counter))

;;; Buffer Management for Line-Based Protocols

(defun mcp-server-transport--create-line-buffer ()
  "Create a line buffer for accumulating partial messages."
  "")

(defun mcp-server-transport--process-buffer-lines (buffer new-data line-processor)
  "Process lines in BUFFER with NEW-DATA using LINE-PROCESSOR.
Returns updated buffer with remaining partial data."
  (let ((combined (concat buffer new-data))
        (remaining ""))
    
    ;; Process complete lines
    (while (string-match "\n" combined)
      (let* ((line-end (match-end 0))
             (line (substring combined 0 (1- line-end))))
        
        ;; Remove processed line from combined buffer
        (setq combined (substring combined line-end))
        
        ;; Process the line if it's not empty
        (when (> (length (string-trim line)) 0)
          (condition-case err
              (funcall line-processor line)
            (error
             (message "Error processing line: %s" (error-message-string err)))))))
    
    ;; Return remaining partial data
    combined))

(provide 'mcp-server-transport)

;;; mcp-server-transport.el ends here