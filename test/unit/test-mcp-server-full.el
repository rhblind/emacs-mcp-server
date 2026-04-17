;;; test-mcp-server-full.el --- Full MCP Server Integration Tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests that load and test the complete MCP server.

;;; Code:

(require 'ert)
(require 'test-helpers)

;; Load the complete MCP server
(require 'mcp-server)

;;; Server Loading and Constants Tests

(ert-deftest mcp-test-server-loads-successfully ()
  "Test that the MCP server loads without errors."
  (should (featurep 'mcp-server))
  (should (featurep 'mcp-server-tools))
  (should (featurep 'mcp-server-security))
  (should (featurep 'mcp-server-transport)))

(ert-deftest mcp-test-server-constants ()
  "Test that server constants are properly defined."
  (should (boundp 'mcp-server-version))
  (should (stringp mcp-server-version))
  (should (string-match-p "^[0-9]+\\.[0-9]+\\.[0-9]+$" mcp-server-version))
  
  (should (boundp 'mcp-server-protocol-version))
  (should (stringp mcp-server-protocol-version))
  (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" mcp-server-protocol-version)))

(ert-deftest mcp-test-server-variables ()
  "Test that server variables are properly initialized."
  (should (boundp 'mcp-server-running))
  (should (boundp 'mcp-server-current-transport))
  (should (boundp 'mcp-server-debug))
  (should (boundp 'mcp-server-default-transport)))

(ert-deftest mcp-test-server-capabilities ()
  "Server must only advertise capabilities it actually implements.
Issue #14: advertising resources/prompts/listChanged we don't emit
causes clients to wait forever or otherwise misbehave."
  (should (boundp 'mcp-server-capabilities))
  ;; Tools is implemented, must be advertised
  (should (assq 'tools mcp-server-capabilities))
  ;; Resources not implemented (tracked in #12) - must not be advertised
  (should-not (assq 'resources mcp-server-capabilities))
  ;; Prompts not implemented - must not be advertised
  (should-not (assq 'prompts mcp-server-capabilities))
  ;; tools.listChanged notification is never emitted - must not be claimed
  (let ((tools-cap (alist-get 'tools mcp-server-capabilities)))
    (should (or (null tools-cap)
                (and (hash-table-p tools-cap)
                     (zerop (hash-table-count tools-cap)))))))

(ert-deftest mcp-test-server-capabilities-serialize-empty-tools-object ()
  "Capabilities must serialize as {\"tools\":{}} for MCP spec compliance.
Empty object indicates tools exist without optional sub-capabilities."
  (let* ((converted (mcp-server-transport--alist-to-json mcp-server-capabilities))
         (json-str (json-serialize converted)))
    (should (string= json-str "{\"tools\":{}}"))))

(ert-deftest mcp-test-no-unimplemented-method-handlers ()
  "Handlers for unsupported methods must not exist.
Issue #14: resources and prompts handlers were stubs that lied about
support; removing them lets the method router return -32601 (method
not found) which is the correct response."
  (should-not (fboundp 'mcp-server--handle-resources-list))
  (should-not (fboundp 'mcp-server--handle-resources-read))
  (should-not (fboundp 'mcp-server--handle-prompts-list)))

(ert-deftest mcp-test-unimplemented-methods-return-method-not-found ()
  "Requests for unadvertised methods must return JSON-RPC -32601.
Issue #14: after removing the stub handlers for `resources/list',
`resources/read', and `prompts/list', the message router must fall
through to the catch-all branch that emits `Method not found'."
  (dolist (method '("resources/list" "resources/read" "prompts/list"))
    (let ((captured nil))
      (cl-letf (((symbol-function 'mcp-server-transport-send)
                 (lambda (_transport _client-id response)
                   (setq captured response))))
        (mcp-server--handle-message
         `((jsonrpc . "2.0") (id . 42) (method . ,method))
         "test-client"))
      (should captured)
      (let ((err (alist-get 'error captured)))
        (should err)
        (should (= (alist-get 'code err) -32601))
        (should (string= (alist-get 'message err) "Method not found"))
        (should (string= (alist-get 'data err) method))))))

;;; Server Function Tests

(ert-deftest mcp-test-server-functions-exist ()
  "Test that core server functions exist."
  (should (fboundp 'mcp-server-start))
  (should (fboundp 'mcp-server-start-unix))
  (should (fboundp 'mcp-server-stop))
  (should (fboundp 'mcp-server-restart)))

(ert-deftest mcp-test-server-state-functions ()
  "Test server state management functions."
  ;; Server should start in stopped state
  (should-not mcp-server-running)
  (should-not mcp-server-current-transport))

;;; Tools Integration Tests

(ert-deftest mcp-test-tools-integration ()
  "Test that tools system integrates with server."
  (should (fboundp 'mcp-server-tools-register))
  (should (fboundp 'mcp-server-tools-list))
  (should (fboundp 'mcp-server-tools-get))
  (should (fboundp 'mcp-server-tools-exists-p))
  
  ;; Test tool registration works
  (let ((mcp-server-tools--registry (make-hash-table :test 'equal)))
    (mcp-server-tools-register
     "integration-test-tool"
     "Integration Test Tool" 
     "Tool for integration testing"
     '((type . "object"))
     (lambda (args) "integration-result"))
    
    (should (mcp-server-tools-exists-p "integration-test-tool"))
    (let ((tools (mcp-server-tools-list)))
      (should (= (length tools) 1))
      (should (equal (alist-get 'name (car tools)) "integration-test-tool")))))

;;; Security Integration Tests

(ert-deftest mcp-test-security-integration ()
  "Test that security system integrates with server."
  (should (fboundp 'mcp-server-security-check-permission))
  (should (boundp 'mcp-server-security-dangerous-functions))
  (should (boundp 'mcp-server-security-prompt-for-permissions)))

;;; Transport Integration Tests

(ert-deftest mcp-test-transport-integration ()
  "Test that transport system is available."
  (should (fboundp 'mcp-server-transport-start))
  (should (fboundp 'mcp-server-transport-stop))
  (should (fboundp 'mcp-server-transport-register)))

;;; Configuration Tests

(ert-deftest mcp-test-socket-configuration ()
  "Test socket configuration variables."
  (should (boundp 'mcp-server-socket-name))
  (should (boundp 'mcp-server-socket-directory))
  (should (boundp 'mcp-server-socket-conflict-resolution)))

(ert-deftest mcp-test-customization-group ()
  "Test that customization group exists."
  (should (get 'mcp-server 'group-documentation)))

;;; Basic Server Lifecycle Test (Safe)

(ert-deftest mcp-test-server-debug-toggle ()
  "Test debug mode toggle (safe operation)."
  (let ((original-debug mcp-server-debug))
    (unwind-protect
        (progn
          (setq mcp-server-debug t)
          (should mcp-server-debug)
          (setq mcp-server-debug nil)
          (should-not mcp-server-debug))
      (setq mcp-server-debug original-debug))))

(provide 'test-mcp-server-full)
;;; test-mcp-server-full.el ends here