;;; test-mcp-protocol.el --- Tests for MCP Protocol Implementation -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for MCP protocol message handling and JSON-RPC compliance.

;;; Code:

(require 'ert)
(require 'test-helpers)
(require 'json)

;; Load MCP server modules safely
(condition-case nil
    (progn
      (require 'mcp-server-tools)
      (require 'mcp-server-security)  
      (require 'mcp-server-transport)
      (require 'mcp-server))
  (error 
   (message "Warning: Some MCP server modules not available")))

;;; JSON-RPC Message Parsing Tests

(ert-deftest mcp-test-json-rpc-valid-request ()
  "Test parsing of valid JSON-RPC request."
  (let* ((request (mcp-test-make-jsonrpc-request "test-1" "tools/list"))
         (json-str (mcp-test-json-encode request))
         (parsed (mcp-test-json-decode json-str)))
    (mcp-test-should-be-valid-jsonrpc parsed)
    (should (equal (alist-get 'id parsed) "test-1"))
    (should (equal (alist-get 'method parsed) "tools/list"))))

(ert-deftest mcp-test-json-rpc-request-with-params ()
  "Test parsing of JSON-RPC request with parameters."
  (let* ((params '((name . "eval-elisp") (arguments . ((expression . "(+ 1 2)")))))
         (request (mcp-test-make-jsonrpc-request "test-2" "tools/call" params))
         (json-str (mcp-test-json-encode request))
         (parsed (mcp-test-json-decode json-str)))
    (mcp-test-should-be-valid-jsonrpc parsed)
    (should (alist-get 'params parsed))
    (should (equal (alist-get 'name (alist-get 'params parsed)) "eval-elisp"))))

(ert-deftest mcp-test-json-rpc-malformed-request ()
  "Test handling of malformed JSON-RPC request."
  (let ((invalid-json "{\"jsonrpc\": \"2.0\", \"method\": \"test\", \"id\":"))
    (should-error (mcp-test-json-decode invalid-json))))

(ert-deftest mcp-test-json-rpc-missing-fields ()
  "Test handling of JSON-RPC request with missing required fields."
  (let* ((incomplete-request '((jsonrpc . "2.0") (method . "test")))
         (json-str (mcp-test-json-encode incomplete-request))
         (parsed (mcp-test-json-decode json-str)))
    (should-not (alist-get 'id parsed))))

;;; MCP Protocol Initialize Tests

(ert-deftest mcp-test-initialize-request ()
  "Test MCP initialize request handling."
  (mcp-test-with-mock-server
   (let* ((params '((protocolVersion . "2024-11-05")
                    (capabilities . ((tools . ((listChanged . t)))))
                    (clientInfo . ((name . "test-client") (version . "1.0.0")))))
          (request (mcp-test-make-jsonrpc-request "init-1" "initialize" params)))
     (let ((result (mcp-server--handle-initialize "init-1" (alist-get 'params request) "test-client")))
       (mcp-test-should-be-valid-mcp-initialize-result result)
       (should (equal (alist-get 'protocolVersion result) mcp-server-protocol-version))
       (should (alist-get 'capabilities result))
       (should (alist-get 'serverInfo result))))))

(ert-deftest mcp-test-initialize-unsupported-version ()
  "Test handling of unsupported protocol version."
  (mcp-test-with-mock-server
   (let* ((params '((protocolVersion . "1999-01-01")
                    (capabilities . ((tools . ((listChanged . t)))))
                    (clientInfo . ((name . "test-client") (version . "1.0.0")))))
          (request (mcp-test-make-jsonrpc-request "init-2" "initialize" params)))
     (should-error (mcp-server--handle-initialize "init-2" (alist-get 'params request) "test-client")))))

(ert-deftest mcp-test-initialize-missing-client-info ()
  "Test initialize request with missing client info."
  (mcp-test-with-mock-server
   (let* ((params '((protocolVersion . "2024-11-05")
                    (capabilities . ((tools . ((listChanged . t)))))))
          (request (mcp-test-make-jsonrpc-request "init-3" "initialize" params)))
     (should-error (mcp-server--handle-initialize "init-3" (alist-get 'params request) "test-client")))))

;;; Tools List Tests

(ert-deftest mcp-test-tools-list-empty ()
  "Test tools/list with no registered tools."
  (mcp-test-with-mock-server
   (let ((result (mcp-server--handle-tools-list "tools-1" nil "test-client")))
     (should (alist-get 'tools result))
     (should (equal (alist-get 'tools result) '())))))

(ert-deftest mcp-test-tools-list-with-tools ()
  "Test tools/list with registered tools."
  (mcp-test-with-mock-server
   (mcp-test-register-mock-tool "mock-tool-1")
   (mcp-test-register-mock-tool "mock-tool-2")
   (let ((result (mcp-server--handle-tools-list "tools-2" nil "test-client")))
     (should (alist-get 'tools result))
     (let ((tools (alist-get 'tools result)))
       (should (= (length tools) 2))
       (mcp-test-should-be-valid-tool-list tools)))))

;;; Tools Call Tests

(ert-deftest mcp-test-tools-call-valid ()
  "Test valid tools/call request."
  (mcp-test-with-mock-server
   (mcp-test-register-mock-tool "test-tool")
   (let* ((params '((name . "test-tool")
                    (arguments . ((input . "test-input")))))
          (result (mcp-server--handle-tools-call "call-1" params "test-client")))
     (should (alist-get 'content result))
     (should (stringp (caar (alist-get 'content result)))))))

(ert-deftest mcp-test-tools-call-nonexistent ()
  "Test tools/call with nonexistent tool."
  (mcp-test-with-mock-server
   (let* ((params '((name . "nonexistent-tool")
                    (arguments . ((input . "test-input"))))))
     (should-error (mcp-server--handle-tools-call "call-2" params "test-client")))))

(ert-deftest mcp-test-tools-call-invalid-args ()
  "Test tools/call with invalid arguments."
  (mcp-test-with-mock-server
   (mcp-test-register-mock-tool "test-tool")
   (let* ((params '((name . "test-tool")
                    (arguments . ((invalid-param . "value"))))))
     (should-error (mcp-server--handle-tools-call "call-3" params "test-client")))))

;;; Message Handling Tests

(ert-deftest mcp-test-handle-unknown-method ()
  "Test handling of unknown method."
  (mcp-test-with-mock-server
   (let* ((request (mcp-test-make-jsonrpc-request "unknown-1" "unknown/method")))
     (should-error (mcp-server--handle-message request "test-client")))))

(ert-deftest mcp-test-handle-notification ()
  "Test handling of JSON-RPC notification (no id)."
  (mcp-test-with-mock-server
   (let* ((notification '((jsonrpc . "2.0") (method . "initialized"))))
     (should-not (mcp-server--handle-message notification "test-client")))))

(ert-deftest mcp-test-handle-malformed-message ()
  "Test handling of malformed message."
  (mcp-test-with-mock-server
   (let* ((malformed '((invalid . "message"))))
     (should-error (mcp-server--handle-message malformed "test-client")))))

;;; Protocol Version Tests

(ert-deftest mcp-test-protocol-version-constant ()
  "Test that protocol version constant is properly defined."
  (should (stringp mcp-server-protocol-version))
  (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" mcp-server-protocol-version)))

(ert-deftest mcp-test-server-version-constant ()
  "Test that server version constant is properly defined."
  (should (stringp mcp-server-version))
  (should (string-match-p "^[0-9]+\\.[0-9]+\\.[0-9]+$" mcp-server-version)))

;;; Capabilities Tests

(ert-deftest mcp-test-server-capabilities ()
  "Test that server capabilities are properly defined."
  (should (alist-get 'tools mcp-server-capabilities))
  (should (alist-get 'resources mcp-server-capabilities))
  (should (alist-get 'prompts mcp-server-capabilities)))

;;; Error Response Tests

(ert-deftest mcp-test-error-response-format ()
  "Test that error responses follow JSON-RPC format."
  (let* ((error-code -32601)
         (error-message "Method not found")
         (error-response (mcp-test-make-jsonrpc-error "error-1" error-code error-message)))
    (mcp-test-should-be-valid-jsonrpc error-response)
    (should (alist-get 'error error-response))
    (should (equal (alist-get 'code (alist-get 'error error-response)) error-code))
    (should (equal (alist-get 'message (alist-get 'error error-response)) error-message))))

;;; Resources and Prompts Tests (Basic Structure)

(ert-deftest mcp-test-resources-list-empty ()
  "Test resources/list with no resources."
  (mcp-test-with-mock-server
   (let ((result (mcp-server--handle-resources-list "res-1" nil "test-client")))
     (should (alist-get 'resources result))
     (should (equal (alist-get 'resources result) '())))))

(ert-deftest mcp-test-prompts-list-empty ()
  "Test prompts/list with no prompts."
  (mcp-test-with-mock-server
   (let ((result (mcp-server--handle-prompts-list "prompt-1" nil "test-client")))
     (should (alist-get 'prompts result))
     (should (equal (alist-get 'prompts result) '())))))

(provide 'test-mcp-protocol)
;;; test-mcp-protocol.el ends here