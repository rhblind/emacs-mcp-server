;;; mcp-server.el --- Pure Elisp MCP Server -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude Code + Rolf Håvard Blindheim<rhblind@gmail.com>
;; URL: https://github.com/rhblind/emacs-mcp-server
;; Keywords: mcp, protocol, integration, tools
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package implements a pure Elisp MCP (Model Context Protocol) server
;; that enables direct integration between LLMs and Emacs internals.
;;
;; The server exposes Emacs functionality through MCP tools, allowing LLMs
;; to execute elisp code, manipulate buffers, navigate projects, and perform
;; any operation expressible in elisp.
;;
;; Key features:
;; - Full MCP protocol compliance
;; - Multiple transport backends (Unix sockets, TCP, stdio)
;; - Safe execution sandbox with permission controls
;; - Direct access to Emacs state and functionality
;; - Comprehensive tool registry for elisp functions
;; - Multi-client support with concurrent connections
;;
;; Usage:
;;   (require 'mcp-server)
;;   (mcp-server-start-unix)    ; Start with Unix domain socket
;;   (mcp-server-start-tcp)     ; Start with TCP socket (future)
;;   (mcp-server-start)         ; Start with default transport (Unix)
;;
;; The server supports multiple transport mechanisms for maximum flexibility
;; in LLM integration scenarios.

;;; Code:

(require 'json)
(require 'mcp-server-transport)
(require 'mcp-server-transport-unix)
(require 'mcp-server-transport-tcp)
(require 'mcp-server-tools)
(require 'mcp-server-security)
(require 'mcp-server-emacs-tools)

;;; Constants

(defconst mcp-server-version "0.1.0"
  "Version of the Emacs MCP server.")

(defconst mcp-server-protocol-version "2024-11-05"
  "MCP protocol version supported by this server.")

;;; Variables

(defvar mcp-server-current-transport nil
  "Currently active transport name.")

(defvar mcp-server-running nil
  "Whether the MCP server is currently running.")

(defvar mcp-server-debug nil
  "Whether to enable debug logging.")

(defvar mcp-server-default-transport "unix"
  "Default transport to use when none is specified.")

;;; Customization Group

(defgroup mcp-server nil
  "Emacs MCP Server configuration."
  :group 'external
  :prefix "mcp-server-")

;;; Socket Naming Configuration

(defcustom mcp-server-socket-name nil
  "Socket name configuration for Unix domain sockets.

This controls how the socket file is named:

- nil (default): Use simple default naming (emacs-mcp-server.sock)
- string: Use as socket name (emacs-mcp-server-{string-value}.sock)
- 'user: Use username-based naming (emacs-mcp-server-{username}.sock)
- 'session: Use session-based naming for multiple instances (emacs-mcp-server-{username}-{pid}.sock)
- function: Call function to generate socket name dynamically

Examples:
(setq mcp-server-socket-name nil)                      ; Default: emacs-mcp-server.sock
(setq mcp-server-socket-name \"my-instance\")          ; Custom name
(setq mcp-server-socket-name 'user)                    ; User-based
(setq mcp-server-socket-name 'session)                 ; Session-based
(setq mcp-server-socket-name
      (lambda () (format \"emacs-%s\" (system-name)))) ; Dynamic"
  :type '(choice (const :tag "Default (emacs-mcp-server.sock)" nil)
          (string :tag "Fixed socket name")
          (const :tag "Username-based" user)
          (const :tag "Session-based" session)
          (function :tag "Dynamic function"))
  :group 'mcp-server)

(defcustom mcp-server-socket-directory user-emacs-directory
  "Directory for socket files.
Defaults to `user-emacs-directory'. Users can customize this with:
  (setq mcp-server-socket-directory \"~/my-socket-dir\")"
  :type 'directory
  :group 'mcp-server)

(defcustom mcp-server-socket-conflict-resolution 'warn
  "How to handle socket name conflicts.

- 'warn: Warn user and use alternative naming
- 'error: Throw error and refuse to start
- 'force: Remove existing socket and proceed (dangerous)
- 'auto: Automatically append suffix to avoid conflicts"
  :type '(choice (const :tag "Warn and use alternative" warn)
          (const :tag "Error and refuse to start" error)
          (const :tag "Force removal (dangerous)" force)
          (const :tag "Auto-append suffix" auto))
  :group 'mcp-server)

(defvar mcp-server-capabilities
  '((tools . ((listChanged . t)))
    (resources . ((subscribe . t) (listChanged . t)))
    (prompts . ((listChanged . t))))
  "Capabilities supported by this MCP server.")

;;; Logging

(defun mcp-server--log (level message &rest args)
  "Log MESSAGE with LEVEL and ARGS if debugging is enabled."
  (when mcp-server-debug
    (let ((formatted-message (apply #'format message args)))
      (message "[MCP %s] %s" level formatted-message))))

(defun mcp-server--debug (message &rest args)
  "Log debug MESSAGE with ARGS."
  (apply #'mcp-server--log "DEBUG" message args))

(defun mcp-server--info (message &rest args)
  "Log info MESSAGE with ARGS."
  (apply #'mcp-server--log "INFO" message args))

(defun mcp-server--error (message &rest args)
  "Log error MESSAGE with ARGS."
  (apply #'mcp-server--log "ERROR" message args))

;;; Server Management

;;;###autoload
(defun mcp-server-start (&optional debug transport)
  "Start the MCP server with optional DEBUG and TRANSPORT.
TRANSPORT defaults to 'unix' if not specified.
If DEBUG is non-nil, enable debug logging."
  (interactive "P")
  (let ((transport-name (or transport mcp-server-default-transport)))
    (mcp-server--start-with-transport transport-name debug)))

;;;###autoload
(defun mcp-server-start-unix (&optional debug socket-path)
  "Start MCP server with Unix domain socket transport.
If DEBUG is non-nil, enable debug logging.
SOCKET-PATH specifies custom socket location."
  (interactive "P")
  (mcp-server--start-with-transport "unix" debug socket-path))

;;;###autoload
(defun mcp-server-start-tcp (&optional debug host port)
  "Start MCP server with TCP transport.
If DEBUG is non-nil, enable debug logging.
HOST and PORT specify the bind address (planned for future implementation)."
  (interactive "P")
  (mcp-server--start-with-transport "tcp" debug host port))

(defun mcp-server--start-with-transport (transport-name debug &rest args)
  "Start MCP server with TRANSPORT-NAME, DEBUG flag and ARGS."
  (when mcp-server-running
    (error "MCP server is already running"))
  
  (setq mcp-server-debug debug)
  (setq mcp-server-current-transport transport-name)
  
  (mcp-server--info "Starting MCP server (version %s) with %s transport" 
                    mcp-server-version transport-name)

  ;; Initialize components
  (mcp-server-tools-init)
  (mcp-server-security-init)
  (mcp-server-emacs-tools-register)
  
  ;; Start the transport
  (condition-case err
      (progn
        (apply #'mcp-server-transport-start 
               transport-name #'mcp-server--handle-message args)
        (setq mcp-server-running t)
        (mcp-server--info "MCP server started successfully"))
    (error
     (mcp-server--error "Failed to start server: %s" (error-message-string err))
     (error "Failed to start MCP server: %s" (error-message-string err)))))

(defun mcp-server-stop ()
  "Stop the MCP server."
  (interactive)
  (unless mcp-server-running
    (error "MCP server is not running"))
  
  (mcp-server--info "Stopping MCP server")
  
  ;; Stop the transport
  (when mcp-server-current-transport
    (mcp-server-transport-stop mcp-server-current-transport))
  
  ;; Cleanup components
  (mcp-server-tools-cleanup)
  (mcp-server-security-cleanup)
  
  (setq mcp-server-running nil)
  (setq mcp-server-current-transport nil)
  
  (mcp-server--info "MCP server stopped"))

(defun mcp-server-restart (&optional debug)
  "Restart the MCP server.
If DEBUG is non-nil, enable debug logging."
  (interactive "P")
  (let ((old-transport mcp-server-current-transport))
    (when mcp-server-running
      (mcp-server-stop))
    (mcp-server--start-with-transport (or old-transport mcp-server-default-transport) debug)))

;;; Message Handling

(defun mcp-server--handle-message (message &optional client-id)
  "Handle incoming MCP MESSAGE from optional CLIENT-ID."
  (mcp-server--debug "Handling message from %s: %s" (or client-id "unknown") message)
  
  (condition-case err
      (catch 'mcp-handled
        (let ((method (alist-get 'method message))
              (id (alist-get 'id message))
              (params (alist-get 'params message)))

          (cond
           ;; Initialize request
           ((string= method "initialize")
            (mcp-server--handle-initialize id params client-id))

           ;; Tools
           ((string= method "tools/list")
            (mcp-server--handle-tools-list id params client-id))

           ((string= method "tools/call")
            (mcp-server--debug "About to call tools/call handler")
            (let ((result (mcp-server--handle-tools-call id params client-id)))
              (mcp-server--debug "tools/call handler returned: %S" result)
              result))

           ;; Resources (future implementation)
           ((string= method "resources/list")
            (mcp-server--handle-resources-list id params client-id))

           ((string= method "resources/read")
            (mcp-server--handle-resources-read id params client-id))

           ;; Prompts (future implementation)
           ((string= method "prompts/list")
            (mcp-server--handle-prompts-list id params client-id))

           ;; Notifications
           ((string= method "notifications/initialized")
            (mcp-server--handle-initialized))

           ;; Unknown method
           (t
            (mcp-server--send-error client-id id -32601 "Method not found" method)))))
    
    (error
     (mcp-server--debug "Main error handler - err=%S, message=%S" err message)
     (mcp-server--debug "Message id=%S" (alist-get 'id message))
     (mcp-server--error "Error handling message: %s" err)
     (condition-case send-err
         (mcp-server--send-error client-id (alist-get 'id message) -32603 "Internal error" (error-message-string err))
       (error
        (mcp-server--debug "Error in send-error: %s" (error-message-string send-err)))))))

(defun mcp-server--handle-initialize (id params client-id)
  "Handle initialize request with ID and PARAMS from CLIENT-ID."
  (mcp-server--debug "Initialize request from %s: %s" client-id params)
  
  (let ((protocol-version (alist-get 'protocolVersion params))
        (client-capabilities (alist-get 'capabilities params))
        (client-info (alist-get 'clientInfo params)))
    
    (mcp-server--info "Client %s connecting: %s" client-id (alist-get 'name client-info))
    
    ;; Send initialize response
    (mcp-server--send-response
     client-id id
     `((protocolVersion . ,mcp-server-protocol-version)
       (capabilities . ,mcp-server-capabilities)
       (serverInfo . ((name . "mcp-server")
                      (title . "Emacs MCP Server")
                      (version . ,mcp-server-version)))
       (instructions . "This server provides direct access to Emacs functionality through MCP tools. Use eval-elisp to execute arbitrary elisp code.")))))

(defun mcp-server--handle-initialized ()
  "Handle initialized notification."
  (mcp-server--info "Client initialization complete"))

(defun mcp-server--handle-tools-list (id params client-id)
  "Handle tools/list request with ID and PARAMS from CLIENT-ID."
  (mcp-server--debug "Tools list request from %s: %s" client-id params)
  
  (let ((tools (mcp-server-tools-list)))
    (mcp-server--send-response
     client-id id
     `((tools . ,tools)))))

(defun mcp-server--handle-tools-call (id params client-id)
  "Handle tools/call request with ID and PARAMS from CLIENT-ID."
  (mcp-server--debug "Tools call request from %s: %s" client-id params)
  
  (let ((tool-name (alist-get 'name params))
        (arguments (alist-get 'arguments params)))
    
    (condition-case err
        (let* ((result (mcp-server-tools-call tool-name arguments))
               ;; Check if this is an error result
               (is-error-bool (and (> (length result) 0)
                                   (listp (aref result 0))
                                   (eq (alist-get 'type (aref result 0)) 'error))))
          (mcp-server--debug "Tool %s - is-error-bool = %S (type: %s)" 
                             tool-name is-error-bool (type-of is-error-bool))
          ;; Use direct hash table approach to avoid alist conversion issues
          (condition-case direct-err
              (let ((response-hash (make-hash-table :test 'equal))
                    (result-hash (make-hash-table :test 'equal)))
                ;; Build result hash
                (puthash "content" (vconcat (append result nil)) result-hash)
                (puthash "isError" (if is-error-bool t :false) result-hash)
                ;; Build response hash
                (puthash "jsonrpc" "2.0" response-hash)
                (puthash "id" id response-hash)
                (puthash "result" result-hash response-hash)
                ;; Send using raw JSON
                (let ((json-str (json-serialize response-hash)))
                  (mcp-server--debug "Direct JSON: %s" json-str)
                  ;; Send the JSON directly by getting the client process
                  (let* ((client-data (mcp-server-transport-unix--get-client client-id))
                         (process (car client-data)))
                    (if (and process (eq (process-status process) 'open))
                        (progn
                          (process-send-string process (concat json-str "\n"))
                          (mcp-server--debug "Direct send completed successfully")
                          ;; Exit cleanly without returning to main handler
                          (throw 'mcp-handled 'success))
                      (error "Client process not available for direct send")))))
            (error
             ;; If direct approach fails, fall back to error response
             (mcp-server--debug "Direct send failed: %s" (error-message-string direct-err))
             (error "Direct send failed: %s" (error-message-string direct-err))))))

    (error
     (mcp-server--send-response
      client-id id
      `((content . (((type . "text")
                     (text . "Tool execution failed"))))
        (isError . t))))))

(defun mcp-server--handle-resources-list (id params client-id)
  "Handle resources/list request with ID and PARAMS from CLIENT-ID.
Returns empty list as resources feature is planned for future implementation."
  (mcp-server--debug "Resources list request from %s: %s" client-id params)
  
  (mcp-server--send-response
   client-id id
   '((resources . []))))

(defun mcp-server--handle-resources-read (id params client-id)
  "Handle resources/read request with ID and PARAMS from CLIENT-ID.
Returns error as resources feature is planned for future implementation."
  (mcp-server--debug "Resources read request from %s: %s" client-id params)
  
  (mcp-server--send-error client-id id -32002 "Resource not found" params))

(defun mcp-server--handle-prompts-list (id params client-id)
  "Handle prompts/list request with ID and PARAMS from CLIENT-ID.
Returns empty list as prompts feature is planned for future implementation."
  (mcp-server--debug "Prompts list request from %s: %s" client-id params)
  
  (mcp-server--send-response
   client-id id
   '((prompts . []))))

(defun mcp-server--send-error (client-id id code message &optional data)
  "Send error response to CLIENT-ID with ID, CODE, MESSAGE and optional DATA."
  (let ((error-response `((jsonrpc . "2.0")
                          (id . ,id)
                          (error . ((code . ,code)
                                    (message . ,message)
                                    ,@(when data `((data . ,data))))))))
    (mcp-server-transport-send mcp-server-current-transport client-id error-response)))

(defun mcp-server--send-response (client-id id result)
  "Send successful response to CLIENT-ID with ID and RESULT."
  (let ((response `((jsonrpc . "2.0")
                    (id . ,id)
                    (result . ,result))))
    (mcp-server--debug "Sending response = %S" response)
    (mcp-server--debug "mcp-server-current-transport = %S" mcp-server-current-transport)
    (mcp-server--debug "client-id = %S (type: %s)" client-id (type-of client-id))
    (mcp-server-transport-send mcp-server-current-transport client-id response)))

(defun mcp-server--send-response-with-bool (client-id id result-content is-error-bool)
  "Send response with explicit boolean handling for isError field.
CLIENT-ID is the client identifier.
ID is the request ID.
RESULT-CONTENT is the content part of the result.
IS-ERROR-BOOL is a boolean indicating if this is an error."
  ;; Create a hash table for the result to ensure proper JSON serialization
  (let* ((result-ht (make-hash-table :test 'equal))
         (response-ht (make-hash-table :test 'equal)))
    ;; Add content to result hash table
    (dolist (pair result-content)
      (let ((key (if (symbolp (car pair))
                     (symbol-name (car pair))
                   (car pair)))
            (value (cdr pair)))
        (mcp-server--debug "Adding to result - key=%S, value=%S (type=%s)" key value (type-of value))
        ;; Convert the value properly for JSON serialization
        (when (string= key "content")
          (cond
           ;; If it's a list of alists, convert each alist to hash table
           ((and (listp value) (seq-every-p #'listp value))
            (mcp-server--debug "Converting content list of alists: %S" value)
            (setq value (vconcat (mapcar (lambda (item)
                                           (let ((ht (make-hash-table :test 'equal)))
                                             (dolist (pair item)
                                               (puthash (symbol-name (car pair)) (cdr pair) ht))
                                             ht))
                                         value)))
            (mcp-server--debug "Converted content to: %S" value))
           ;; If it's just a list, convert to vector
           ((listp value)
            (setq value (vconcat value)))))
        (puthash key value result-ht)))
    ;; Add isError field with proper boolean value
    (puthash "isError" (if is-error-bool t :false) result-ht)
    ;; Build response hash table
    (puthash "jsonrpc" "2.0" response-ht)
    (puthash "id" id response-ht)
    (puthash "result" result-ht response-ht)
    
    (mcp-server--debug "Response hash table - isError=%S, response-ht=%S" 
                       is-error-bool response-ht)

    ;; Serialize and send using json-serialize directly
    (condition-case err
        (let ((json-str (json-serialize response-ht)))
          (mcp-server--debug "Final JSON: %s" json-str)
          (mcp-server-transport-send-raw "unix" client-id json-str))
      (error
       (mcp-server--debug "Error in response-with-bool: %s" (error-message-string err))
       ;; Fallback to normal response
       (let ((content-value (alist-get 'content result-content)))
         (mcp-server--send-response client-id id 
                                    `((content . ,content-value)
                                      (isError . ,(if is-error-bool t :false)))))))))

;;; Interactive Commands

;;;###autoload
(defun mcp-server-toggle-debug ()
  "Toggle debug logging for the MCP server."
  (interactive)
  (setq mcp-server-debug (not mcp-server-debug))
  (message "MCP server debug logging %s" (if mcp-server-debug "enabled" "disabled")))

;;;###autoload
(defun mcp-server-status ()
  "Show the status of the MCP server."
  (interactive)
  (if mcp-server-running
      (let ((transport-status (mcp-server-transport-status mcp-server-current-transport))
            (client-count (length (mcp-server-transport-list-clients mcp-server-current-transport))))
        (message "MCP server is running with %s transport (debug: %s, clients: %d)\nTransport status: %s" 
                 mcp-server-current-transport
                 (if mcp-server-debug "on" "off")
                 client-count
                 transport-status))
    (message "MCP server is stopped (debug: %s)" 
             (if mcp-server-debug "on" "off"))))

;;; Additional Status Commands

;;;###autoload
(defun mcp-server-list-clients ()
  "List all connected MCP clients."
  (interactive)
  (if mcp-server-running
      (let ((clients (mcp-server-transport-list-clients mcp-server-current-transport)))
        (if clients
            (message "Connected clients: %s" 
                     (mapcar (lambda (client) (alist-get 'id client)) clients))
          (message "No clients connected")))
    (message "MCP server is not running")))

;;;###autoload
(defun mcp-server-get-socket-path ()
  "Get the current Unix socket path if using Unix transport."
  (interactive)
  (if (and mcp-server-running 
           (string= mcp-server-current-transport "unix"))
      (let ((socket-path (mcp-server-transport-unix-socket-path)))
        (message "Unix socket path: %s" socket-path)
        socket-path)
    (message "Server not running or not using Unix transport")))

;;;###autoload
(defun mcp-server-disconnect-client (client-id)
  "Disconnect a specific CLIENT-ID."
  (interactive "sClient ID to disconnect: ")
  (if mcp-server-running
      (if (mcp-server-transport-disconnect-client mcp-server-current-transport client-id)
          (message "Client %s disconnected" client-id)
        (message "Client %s not found" client-id))
    (message "MCP server is not running")))

;;; Socket Name Management Commands

;;;###autoload
(defun mcp-server-start-unix-named (socket-name &optional debug)
  "Start MCP server with Unix socket using SOCKET-NAME.
SOCKET-NAME can be a string for fixed naming, or 'user/'session for strategies.
If DEBUG is non-nil, enable debug logging."
  (interactive "sSocket name (or 'user/'session): \nP")
  (let ((mcp-server-socket-name socket-name))
    (mcp-server-start-unix debug)))

;;;###autoload  
(defun mcp-server-set-socket-name (socket-name)
  "Set the socket name configuration for future server starts.
SOCKET-NAME can be:
- String: Fixed socket name (e.g., \"primary\")
- 'user: Username-based naming  
- 'session: Session-based naming
- Function: Custom naming function
- nil: Revert to PID-based naming"
  (interactive 
   (list (let ((choice (completing-read 
                        "Socket naming strategy: "
                        '("primary" "user" "session" "custom" "pid-based")
                        nil nil)))
           (cond
            ((string= choice "primary") "primary")
            ((string= choice "user") 'user)
            ((string= choice "session") 'session)
            ((string= choice "pid-based") nil)
            ((string= choice "custom") 
             (read-string "Custom socket name: "))
            (t choice)))))
  
  (setq mcp-server-socket-name socket-name)
  (message "Socket name set to: %s" 
           (cond
            ((stringp socket-name) (format "\"%s\"" socket-name))
            ((eq socket-name 'user) "user-based")
            ((eq socket-name 'session) "session-based")  
            ((null socket-name) "PID-based")
            (t socket-name))))

;;;###autoload
(defun mcp-server-show-socket-config ()
  "Show current socket naming configuration."
  (interactive)
  (let ((config mcp-server-socket-name)
        (directory (or mcp-server-socket-directory "~/.emacs.d/.local/cache/"))
        (conflict-res mcp-server-socket-conflict-resolution))
    
    (message "Socket Configuration:\n  Name: %s\n  Directory: %s\n  Conflict Resolution: %s"
             (cond
              ((stringp config) (format "\"%s\" (fixed)" config))
              ((eq config 'user) "user-based")
              ((eq config 'session) "session-based")
              ((functionp config) "custom function")
              ((null config) "PID-based (default)")
              (t config))
             directory
             conflict-res)))

;;;###autoload
(defun mcp-server-get-predicted-socket-path ()
  "Get the socket path that would be used if server started now."
  (interactive)
  (require 'mcp-server-transport-unix)
  (let ((predicted-path (mcp-server-transport-unix--build-socket-path)))
    (message "Predicted socket path: %s" predicted-path)
    predicted-path))

;;; Entry Point for Subprocess

;;;###autoload
(defun mcp-server-main ()
  "Main entry point for running MCP server as subprocess."
  (interactive)
  ;; Enable debug logging for subprocess mode
  (setq mcp-server-debug t)
  
  ;; Start the server with default transport
  (mcp-server-start t)
  
  ;; Keep the process alive
  (while mcp-server-running
    (sleep-for 0.1)))

(provide 'mcp-server)

;;; mcp-server.el ends here
