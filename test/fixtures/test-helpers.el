;;; test-helpers.el --- Test Helper Functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Common test utilities and fixtures for the MCP server test suite.

;;; Code:

(require 'ert)
(require 'json)
(require 'cl-lib)

;; Simple mocking without el-mock
(defvar mcp-test-mock-functions nil
  "Alist of mocked functions.")

(defmacro mcp-test-with-mock (mock-specs &rest body)
  "Execute BODY with MOCK-SPECS temporarily overriding functions."
  `(let ((original-functions '()))
     (unwind-protect
         (progn
           ,@(mapcar (lambda (spec)
                       `(progn
                          (push (cons ',(car spec) (symbol-function ',(car spec))) original-functions)
                          (fset ',(car spec) ,(cadr spec))))
                     mock-specs)
           ,@body)
       (dolist (orig original-functions)
         (fset (car orig) (cdr orig))))))

;;; Test Configuration

(defvar mcp-test-temp-dir nil
  "Temporary directory for test files.")

(defvar mcp-test-socket-path nil
  "Temporary socket path for testing.")

;;; Fixture Management

(defmacro mcp-test-with-temp-dir (&rest body)
  "Execute BODY with a temporary directory available as `mcp-test-temp-dir'."
  `(let ((mcp-test-temp-dir (make-temp-file "mcp-test-" t)))
     (unwind-protect
         (progn ,@body)
       (when (file-exists-p mcp-test-temp-dir)
         (delete-directory mcp-test-temp-dir t)))))

(defmacro mcp-test-with-temp-socket (&rest body)
  "Execute BODY with a temporary socket path available as `mcp-test-socket-path'."
  `(mcp-test-with-temp-dir
    (let ((mcp-test-socket-path (expand-file-name "test.sock" mcp-test-temp-dir)))
      (unwind-protect
          (progn ,@body)
        (when (file-exists-p mcp-test-socket-path)
          (delete-file mcp-test-socket-path))))))

(defmacro mcp-test-with-mock-server (&rest body)
  "Execute BODY with a mock server environment setup."
  `(let ((mcp-server-tools--registry (make-hash-table :test 'equal))
         (mcp-server-tools--initialized nil)
         (mcp-server-security--permission-cache (make-hash-table :test 'equal))
         (mcp-server-security--audit-log '())
         (mcp-server-security-prompt-for-permissions nil))
     ,@body))

;;; Mock Data Helpers

(defun mcp-test-make-jsonrpc-request (id method &optional params)
  "Create a JSON-RPC request with ID, METHOD and optional PARAMS."
  (let ((request `((jsonrpc . "2.0")
                   (id . ,id)
                   (method . ,method))))
    (when params
      (setq request (append request `((params . ,params)))))
    request))

(defun mcp-test-make-jsonrpc-response (id result)
  "Create a JSON-RPC response with ID and RESULT."
  `((jsonrpc . "2.0")
    (id . ,id)
    (result . ,result)))

(defun mcp-test-make-jsonrpc-error (id code message &optional data)
  "Create a JSON-RPC error response with ID, CODE, MESSAGE and optional DATA."
  (let ((error `((code . ,code)
                 (message . ,message))))
    (when data
      (setq error (append error `((data . ,data)))))
    `((jsonrpc . "2.0")
      (id . ,id)
      (error . ,error))))

(defun mcp-test-json-encode (obj)
  "Encode OBJ as JSON string for testing."
  (json-encode obj))

(defun mcp-test-json-decode (str)
  "Decode JSON string STR for testing."
  (json-read-from-string str))

;;; Mock Client Simulator

(defvar mcp-test-mock-client--received-messages nil
  "List of messages received by mock client.")

(defun mcp-test-mock-client-reset ()
  "Reset mock client state."
  (setq mcp-test-mock-client--received-messages nil))

(defun mcp-test-mock-client-send-message (message)
  "Mock sending MESSAGE to server."
  (push message mcp-test-mock-client--received-messages))

(defun mcp-test-mock-client-get-messages ()
  "Get all messages received by mock client."
  (reverse mcp-test-mock-client--received-messages))

;;; Security Testing Helpers

(defvar mcp-test-permission-responses nil
  "Queue of permission responses for testing.")

(defun mcp-test-mock-permission-prompt (operation data)
  "Mock permission prompt that returns predetermined responses."
  (if mcp-test-permission-responses
      (pop mcp-test-permission-responses)
    t))

(defmacro mcp-test-with-permission-responses (responses &rest body)
  "Execute BODY with predetermined permission RESPONSES."
  `(let ((mcp-test-permission-responses ,responses))
     (cl-letf (((symbol-function 'mcp-server-security--request-permission)
                (lambda (operation data cache-key)
                  (mcp-test-mock-permission-prompt operation data))))
       ,@body)))

;;; Tool Testing Helpers

(defun mcp-test-register-mock-tool (name)
  "Register a mock tool with NAME for testing."
  (mcp-server-tools-register
   name
   (format "Mock Tool %s" name)
   (format "Mock tool %s for testing" name)
   '((type . "object")
     (properties . ((input . ((type . "string")))))
     (required . ["input"]))
   (lambda (args)
     (format "Mock result for %s with input: %s"
             name (alist-get 'input args)))))

;;; Assertion Helpers

(defun mcp-test-should-be-valid-jsonrpc (obj)
  "Assert that OBJ is a valid JSON-RPC object."
  (should (alist-get 'jsonrpc obj))
  (should (equal (alist-get 'jsonrpc obj) "2.0")))

(defun mcp-test-should-be-valid-mcp-initialize-result (result)
  "Assert that RESULT is a valid MCP initialize result."
  (should (alist-get 'protocolVersion result))
  (should (alist-get 'capabilities result))
  (should (alist-get 'serverInfo result)))

(defun mcp-test-should-be-valid-tool-list (tools)
  "Assert that TOOLS is a valid tool list."
  (should (listp tools))
  (dolist (tool tools)
    (should (alist-get 'name tool))
    (should (alist-get 'description tool))
    (should (alist-get 'inputSchema tool))))

;;; File System Helpers

(defun mcp-test-create-temp-file (content &optional suffix)
  "Create temporary file with CONTENT and optional SUFFIX."
  (let ((temp-file (make-temp-file "mcp-test-" nil suffix)))
    (with-temp-file temp-file
      (insert content))
    temp-file))

(defun mcp-test-create-sensitive-file ()
  "Create a file that should be detected as sensitive."
  (mcp-test-create-temp-file "password=secret123\napi_key=abc123"))

;;; Transport Testing Helpers

(defvar mcp-test-transport-messages nil
  "Messages captured during transport testing.")

(defun mcp-test-transport-capture-message (message)
  "Capture MESSAGE during transport testing."
  (push message mcp-test-transport-messages))

(defun mcp-test-transport-get-messages ()
  "Get captured transport messages."
  (reverse mcp-test-transport-messages))

(defun mcp-test-transport-clear-messages ()
  "Clear captured transport messages."
  (setq mcp-test-transport-messages nil))

;;; Coverage Helpers

(defun mcp-test-setup-coverage ()
  "Setup code coverage tracking."
  (when (require 'undercover nil t)
    (undercover "*.el")))

(provide 'test-helpers)
;;; test-helpers.el ends here