;;; mcp-server-transport-unix.el --- Unix Domain Socket Transport -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This module implements Unix domain socket transport for the MCP server.
;; It allows multiple clients to connect simultaneously via Unix sockets.

;;; Code:

(require 'mcp-server-transport)
(require 'cl-lib)

;;; Unix Transport Logging

(defun mcp-server-transport-unix--log (level message &rest args)
  "Log MESSAGE with LEVEL and ARGS if debugging is enabled."
  (when (and (boundp 'mcp-server-debug) mcp-server-debug)
    (let ((formatted-message (apply #'format message args)))
      (message "[MCP UNIX %s] %s" level formatted-message))))

(defun mcp-server-transport-unix--debug (message &rest args)
  "Log debug MESSAGE with ARGS."
  (apply #'mcp-server-transport-unix--log "DEBUG" message args))

(defun mcp-server-transport-unix--info (message &rest args)
  "Log info MESSAGE with ARGS."
  (apply #'mcp-server-transport-unix--log "INFO" message args))

(defun mcp-server-transport-unix--error (message &rest args)
  "Log error MESSAGE with ARGS."
  (apply #'mcp-server-transport-unix--log "ERROR" message args))

;;; Variables

(defvar mcp-server-transport-unix--server-process nil
  "The Unix domain socket server process.")

(defvar mcp-server-transport-unix--socket-path nil
  "Path to the Unix domain socket.")

(defvar mcp-server-transport-unix--clients (make-hash-table :test 'equal)
  "Hash table of connected clients: client-id -> (process . line-buffer).")

(defvar mcp-server-transport-unix--message-handler nil
  "Function to handle incoming messages.")

(defvar mcp-server-transport-unix--running nil
  "Whether the Unix transport is running.")

;;; Socket Path Management

(defun mcp-server-transport-unix--generate-socket-path (&optional custom-path)
  "Generate a Unix socket path using CUSTOM-PATH or configuration."
  (or custom-path
      (mcp-server-transport-unix--build-socket-path)))

(defun mcp-server-transport-unix--get-socket-directory ()
  "Get the best directory for socket files based on platform and configuration."
  (or 
   ;; Use explicit configuration if set
   mcp-server-socket-directory
   
   ;; Try XDG runtime directory (Linux/Unix standard)
   (getenv "XDG_RUNTIME_DIR")
   
   ;; macOS: use user's cache directory
   (when (eq system-type 'darwin)
     (let ((cache-dir "~/Library/Caches/emacs-mcp-server"))
       (when (file-directory-p (expand-file-name "~/Library/Caches"))
         (unless (file-exists-p (expand-file-name cache-dir))
           (make-directory (expand-file-name cache-dir) t))
         (expand-file-name cache-dir))))
   
   ;; Follow Emacs server convention
   (let ((emacs-server-dir (expand-file-name "~/.emacs.d/emacs-mcp-server")))
     (unless (file-exists-p emacs-server-dir)
       (make-directory emacs-server-dir t))
     emacs-server-dir)
   
   ;; Final fallback to /tmp
   "/tmp"))

(defun mcp-server-transport-unix--build-socket-path ()
  "Build socket path based on configuration variables."
  (let* ((base-dir (mcp-server-transport-unix--get-socket-directory))
         (socket-name (mcp-server-transport-unix--resolve-socket-name))
         (socket-path (format "%s/emacs-mcp-server-%s.sock" base-dir socket-name)))
    
    ;; Handle conflicts if socket already exists
    (if (file-exists-p socket-path)
        (mcp-server-transport-unix--handle-socket-conflict socket-path)
      socket-path)))

(defun mcp-server-transport-unix--resolve-socket-name ()
  "Resolve socket name based on configuration."
  (cond
   ;; Function: call it to get dynamic name
   ((functionp mcp-server-socket-name)
    (funcall mcp-server-socket-name))
   
   ;; String: use directly
   ((stringp mcp-server-socket-name)
    mcp-server-socket-name)
   
   ;; Symbol strategies
   ((eq mcp-server-socket-name 'user)
    (user-login-name))
   
   ((eq mcp-server-socket-name 'session)
    (format "%s-%d" (user-login-name) (emacs-pid)))
   
   ;; nil: fallback to PID-based (backward compatibility)
   (t
    (number-to-string (emacs-pid)))))

(defun mcp-server-transport-unix--handle-socket-conflict (socket-path)
  "Handle socket conflict based on configuration."
  (let ((conflict-resolution mcp-server-socket-conflict-resolution)
        (is-stale (mcp-server-transport-unix--is-socket-stale socket-path)))
    
    (cond
     ;; If socket is stale, we can safely remove it
     (is-stale
      (mcp-server-transport-unix--info "Removing stale socket: %s" socket-path)
      (delete-file socket-path)
      socket-path)
     
     ;; Force: remove existing socket (dangerous)
     ((eq conflict-resolution 'force)
      (mcp-server-transport-unix--info "Forcibly removing existing socket: %s" socket-path)
      (delete-file socket-path)
      socket-path)
     
     ;; Error: refuse to start
     ((eq conflict-resolution 'error)
      (error "Socket already exists: %s. Use different name or stop existing server" socket-path))
     
     ;; Auto: append suffix
     ((eq conflict-resolution 'auto)
      (mcp-server-transport-unix--find-alternative-socket-path socket-path))
     
     ;; Warn: notify user and use alternative
     (t
      (mcp-server-transport-unix--info "Socket exists: %s. Using alternative naming." socket-path)
      (mcp-server-transport-unix--find-alternative-socket-path socket-path)))))

(defun mcp-server-transport-unix--is-socket-stale (socket-path)
  "Check if socket is stale (from dead process)."
  (condition-case nil
      ;; Try to connect to see if it's alive
      (let ((test-proc (make-network-process
                        :name "mcp-test"
                        :family 'local
                        :service socket-path
                        :noquery t)))
        (when test-proc
          (delete-process test-proc))
        ;; If we could connect, it's not stale
        nil)
    ;; If connection failed, socket is likely stale
    (error t)))

(defun mcp-server-transport-unix--find-alternative-socket-path (original-path)
  "Find alternative socket path by appending suffix."
  (let ((base (file-name-sans-extension original-path))
        (ext (file-name-extension original-path))
        (counter 1)
        alternative-path)
    
    (while (progn
             (setq alternative-path (format "%s-%d.%s" base counter ext))
             (file-exists-p alternative-path))
      (setq counter (1+ counter)))
    
    alternative-path))

(defun mcp-server-transport-unix--cleanup-socket (socket-path)
  "Clean up socket file at SOCKET-PATH."
  (when (and socket-path (file-exists-p socket-path))
    (condition-case err
        (delete-file socket-path)
      (error
       (mcp-server-transport-unix--error "Warning: Could not delete socket file %s: %s" 
                                         socket-path (error-message-string err))))))

;;; Client Management

(defun mcp-server-transport-unix--add-client (client-id process)
  "Add CLIENT-ID with PROCESS to the client table."
  (puthash client-id 
           (cons process (mcp-server-transport--create-line-buffer))
           mcp-server-transport-unix--clients))

(defun mcp-server-transport-unix--remove-client (client-id)
  "Remove CLIENT-ID from the client table."
  (remhash client-id mcp-server-transport-unix--clients))

(defun mcp-server-transport-unix--get-client (client-id)
  "Get client data for CLIENT-ID."
  (gethash client-id mcp-server-transport-unix--clients))

(defun mcp-server-transport-unix--find-client-by-process (process)
  "Find client ID for PROCESS."
  (let ((client-id nil))
    (maphash (lambda (id client-data)
               (when (eq (car client-data) process)
                 (setq client-id id)))
             mcp-server-transport-unix--clients)
    client-id))

(defun mcp-server-transport-unix--update-client-buffer (client-id new-buffer)
  "Update line buffer for CLIENT-ID to NEW-BUFFER."
  (let ((client-data (mcp-server-transport-unix--get-client client-id)))
    (when client-data
      (puthash client-id (cons (car client-data) new-buffer)
               mcp-server-transport-unix--clients))))

;;; Process Handlers

(defun mcp-server-transport-unix--server-filter (process string)
  "Handle incoming data on server PROCESS with STRING."
  ;; For Unix domain socket servers, new connections are handled via sentinel
  ;; This filter should not normally be called
  (mcp-server-transport-unix--debug "Unexpected data on server process: %s" string))

(defun mcp-server-transport-unix--server-sentinel (process event)
  "Handle server PROCESS sentinel EVENT."
  (cond
   ;; New connection
   ((string-match "open.*" event)
    (mcp-server-transport-unix--handle-new-connection process))
   ;; Server process terminated
   ((memq (process-status process) '(exit signal))
    (mcp-server-transport-unix--error "Unix socket server process terminated: %s" event)
    (mcp-server-transport-unix--stop))
   ;; Other events
   (t
    (mcp-server-transport-unix--debug "Unix socket server process event: %s" event))))

(defun mcp-server-transport-unix--client-filter (process string)
  "Handle incoming data from client PROCESS with STRING."
  (let ((client-id (mcp-server-transport-unix--find-client-by-process process)))
    (when client-id
      (let* ((client-data (mcp-server-transport-unix--get-client client-id))
             (old-buffer (cdr client-data))
             (new-buffer (mcp-server-transport--process-buffer-lines
                          old-buffer
                          string
                          (lambda (line)
                            (mcp-server-transport-unix--process-message client-id line)))))
        (mcp-server-transport-unix--update-client-buffer client-id new-buffer)))))

(defun mcp-server-transport-unix--client-sentinel (process event)
  "Handle client PROCESS sentinel EVENT."
  (let ((client-id (mcp-server-transport-unix--find-client-by-process process)))
    (when client-id
      (mcp-server-transport-unix--info "Client %s disconnected: %s" client-id (string-trim event))
      (mcp-server-transport-unix--remove-client client-id))))

(defun mcp-server-transport-unix--handle-new-connection (client-process)
  "Handle new connection from CLIENT-PROCESS."
  (let ((client-id (mcp-server-transport--generate-client-id)))
    (mcp-server-transport-unix--info "New Unix socket client connected: %s" client-id)
    
    ;; Set up client process
    (set-process-filter client-process #'mcp-server-transport-unix--client-filter)
    (set-process-sentinel client-process #'mcp-server-transport-unix--client-sentinel)
    (set-process-coding-system client-process 'utf-8 'utf-8)
    
    ;; Add to client table
    (mcp-server-transport-unix--add-client client-id client-process)
    
    client-id))

;;; Message Processing

(defun mcp-server-transport-unix--process-message (client-id line)
  "Process a message LINE from CLIENT-ID."
  (condition-case err
      (let ((message (mcp-server-transport--parse-json-rpc line)))
        (mcp-server-transport--validate-json-rpc message)
        
        ;; Add client-id to message context
        (when mcp-server-transport-unix--message-handler
          (funcall mcp-server-transport-unix--message-handler message client-id)))
    (error
     (mcp-server-transport-unix--error "Error processing message from %s: %s" client-id (error-message-string err))
     ;; Send error response to client
     (mcp-server-transport-unix--send-error-to-client 
      client-id nil -32700 "Parse error" (error-message-string err)))))

(defun mcp-server-transport-unix--send-error-to-client (client-id id code message &optional data)
  "Send error response to CLIENT-ID with ID, CODE, MESSAGE and optional DATA."
  ;; Send simple hard-coded error to avoid any recursion issues
  (let* ((client-data (mcp-server-transport-unix--get-client client-id))
         (process (car client-data)))
    (when (and process (eq (process-status process) 'open))
      (let ((simple-error (format "{\"jsonrpc\":\"2.0\",\"id\":%s,\"error\":{\"code\":%d,\"message\":\"%s\"}}\n"
                                  (if id (number-to-string id) "null")
                                  code
                                  message)))
        (process-send-string process simple-error)))))

(defun mcp-server-transport-unix--send-to-client (client-id message)
  "Send MESSAGE to CLIENT-ID."
  (let* ((client-data (mcp-server-transport-unix--get-client client-id))
         (process (car client-data)))
    (when (and process (eq (process-status process) 'open))
      (condition-case err
          (let ((json-string (mcp-server-transport--format-json-rpc message)))
            (process-send-string process (concat json-string "\n")))
        (error
         (mcp-server-transport-unix--error "Critical error sending message to client %s: %s" 
                                           client-id (error-message-string err))
         ;; Send minimal error response to avoid recursion
         (condition-case err2
             (let ((simple-error (format "{\"jsonrpc\":\"2.0\",\"id\":null,\"error\":{\"code\":-32603,\"message\":\"Internal error\"}}\n")))
               (process-send-string process simple-error))
           (error
            (mcp-server-transport-unix--error "Failed to send even simple error response: %s" (error-message-string err2)))))))))

;;; Transport Implementation

(defun mcp-server-transport-unix--start (message-handler &optional socket-path)
  "Start Unix domain socket server with MESSAGE-HANDLER at optional SOCKET-PATH."
  (when mcp-server-transport-unix--running
    (error "Unix transport is already running"))
  
  (setq mcp-server-transport-unix--socket-path 
        (mcp-server-transport-unix--generate-socket-path socket-path))
  
  ;; Clean up any existing socket file
  (mcp-server-transport-unix--cleanup-socket mcp-server-transport-unix--socket-path)
  
  (setq mcp-server-transport-unix--message-handler message-handler)
  (clrhash mcp-server-transport-unix--clients)
  
  (condition-case err
      (progn
        (setq mcp-server-transport-unix--server-process
              (make-network-process
               :name "emacs-mcp-unix-server"
               :family 'local
               :service mcp-server-transport-unix--socket-path
               :server t
               :filter #'mcp-server-transport-unix--server-filter
               :sentinel #'mcp-server-transport-unix--server-sentinel
               :coding 'utf-8))
        
        (setq mcp-server-transport-unix--running t)
        (mcp-server-transport-unix--info "Unix socket MCP server started at: %s" mcp-server-transport-unix--socket-path))
    
    (error
     (mcp-server-transport-unix--cleanup-socket mcp-server-transport-unix--socket-path)
     (error "Failed to start Unix socket server: %s" (error-message-string err)))))

(defun mcp-server-transport-unix--stop ()
  "Stop the Unix domain socket server."
  (when mcp-server-transport-unix--running
    
    ;; Close all client connections
    (maphash (lambda (client-id client-data)
               (let ((process (car client-data)))
                 (when (processp process)
                   (delete-process process))))
             mcp-server-transport-unix--clients)
    (clrhash mcp-server-transport-unix--clients)
    
    ;; Close server process
    (when (processp mcp-server-transport-unix--server-process)
      (delete-process mcp-server-transport-unix--server-process))
    (setq mcp-server-transport-unix--server-process nil)
    
    ;; Clean up socket file
    (mcp-server-transport-unix--cleanup-socket mcp-server-transport-unix--socket-path)
    (setq mcp-server-transport-unix--socket-path nil)
    
    (setq mcp-server-transport-unix--running nil)
    (setq mcp-server-transport-unix--message-handler nil)
    
    (mcp-server-transport-unix--info "Unix socket MCP server stopped")))

(defun mcp-server-transport-unix--send (client-id message)
  "Send MESSAGE to CLIENT-ID."
  (mcp-server-transport-unix--send-to-client client-id message))

(defun mcp-server-transport-unix--send-raw (client-id json-string)
  "Send raw JSON-STRING to CLIENT-ID over Unix socket."
  (let* ((client-data (mcp-server-transport-unix--get-client client-id))
         (process (car client-data)))
    (when (and process (eq (process-status process) 'open))
      (condition-case err
          (process-send-string process (concat json-string "\n"))
        (error
         (mcp-server-transport-unix--error "Error sending raw message to client %s: %s" 
                                           client-id (error-message-string err)))))))

(defun mcp-server-transport-unix--status ()
  "Get status of Unix transport."
  `((running . ,mcp-server-transport-unix--running)
    (socket-path . ,mcp-server-transport-unix--socket-path)
    (client-count . ,(hash-table-count mcp-server-transport-unix--clients))
    (server-process . ,(when mcp-server-transport-unix--server-process
                         (process-status mcp-server-transport-unix--server-process)))))

(defun mcp-server-transport-unix--list-clients ()
  "List all connected clients."
  (let ((clients '()))
    (maphash (lambda (client-id client-data)
               (let ((process (car client-data)))
                 (push `((id . ,client-id)
                         (status . ,(process-status process))
                         (name . ,(process-name process)))
                       clients)))
             mcp-server-transport-unix--clients)
    clients))

(defun mcp-server-transport-unix--disconnect-client (client-id)
  "Disconnect CLIENT-ID."
  (let* ((client-data (mcp-server-transport-unix--get-client client-id))
         (process (car client-data)))
    (when process
      (delete-process process)
      (mcp-server-transport-unix--remove-client client-id)
      t)))

;;; Transport Registration

(defun mcp-server-transport-unix-register ()
  "Register the Unix domain socket transport."
  (mcp-server-transport-register
   "unix"
   (make-mcp-server-transport
    :name "Unix Domain Socket"
    :start-fn #'mcp-server-transport-unix--start
    :stop-fn #'mcp-server-transport-unix--stop
    :send-fn #'mcp-server-transport-unix--send
    :status-fn #'mcp-server-transport-unix--status
    :list-clients-fn #'mcp-server-transport-unix--list-clients
    :disconnect-client-fn #'mcp-server-transport-unix--disconnect-client)))

;; Register on load
(mcp-server-transport-unix-register)

;;; Utility Functions

(defun mcp-server-transport-unix-socket-path ()
  "Get the current Unix socket path."
  mcp-server-transport-unix--socket-path)

(provide 'mcp-server-transport-unix)

;;; mcp-server-transport-unix.el ends here
