;;; test-mcp-server-deferred.el --- Tests for deferred tool response API -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for the async (deferred) tool response infrastructure added to
;; mcp-server.el and mcp-server-tools.el.
;;
;; These tests verify:
;;   - mcp-server-tools-call passes mcp-deferred through unchanged
;;   - mcp-server-tools-call still formats normal results correctly
;;   - mcp-server--current-client-id and mcp-server--current-request-id are
;;     dynamically bound during a tool call so handlers can capture them
;;   - mcp-server-send-tool-result produces correct JSON-RPC success structure
;;   - mcp-server-send-tool-error produces correct JSON-RPC error structure

;;; Code:

(require 'ert)
(require 'test-helpers)
(require 'mcp-server-tools)
(require 'mcp-server)

;;; mcp-server-tools-call deferred pass-through

(ert-deftest mcp-test-deferred-tools-call-returns-mcp-deferred ()
  "mcp-server-tools-call passes mcp-deferred through without formatting."
  (mcp-test-with-mock-server
   (mcp-server-register-tool
    (make-mcp-server-tool
     :name "deferred-test"
     :title "Deferred Test"
     :description "A tool that returns mcp-deferred"
     :input-schema '((type . "object"))
     :function (lambda (_args) 'mcp-deferred)))
   (let ((mcp-server-tools-filter nil))
     (should (eq 'mcp-deferred
                 (mcp-server-tools-call "deferred-test" '()))))))

(ert-deftest mcp-test-deferred-tools-call-formats-normal-result ()
  "mcp-server-tools-call still formats non-deferred results correctly."
  (mcp-test-with-mock-server
   (mcp-server-register-tool
    (make-mcp-server-tool
     :name "normal-test"
     :title "Normal Test"
     :description "A tool that returns a plain string"
     :input-schema '((type . "object"))
     :function (lambda (_args) "hello")))
   (let ((mcp-server-tools-filter nil))
     (let ((result (mcp-server-tools-call "normal-test" '())))
       (should (vectorp result))
       (should (= 1 (length result)))
       (should (equal "text" (alist-get 'type (aref result 0))))
       (should (equal "hello" (alist-get 'text (aref result 0))))))))

(ert-deftest mcp-test-deferred-tools-call-error-still-wrapped ()
  "mcp-server-tools-call wraps handler errors as before."
  (mcp-test-with-mock-server
   (mcp-server-register-tool
    (make-mcp-server-tool
     :name "error-test"
     :title "Error Test"
     :description "A tool that signals an error"
     :input-schema '((type . "object"))
     :function (lambda (_args) (error "boom"))))
   (let ((mcp-server-tools-filter nil))
     (let ((result (mcp-server-tools-call "error-test" '())))
       (should (vectorp result))
       (should (string-match-p "boom" (alist-get 'text (aref result 0))))))))

;;; Dynamic context vars bound during tool call

(ert-deftest mcp-test-deferred-context-vars-bound-during-call ()
  "mcp-server--current-client-id and -request-id are bound during tool call."
  (mcp-test-with-mock-server
   (let (captured-client-id captured-request-id)
     (mcp-server-register-tool
      (make-mcp-server-tool
       :name "capture-test"
       :title "Capture Test"
       :description "Captures the dynamic context vars"
       :input-schema '((type . "object"))
       :function (lambda (_args)
                   (setq captured-client-id  mcp-server--current-client-id
                         captured-request-id mcp-server--current-request-id)
                   'mcp-deferred)))
     ;; Simulate the binding that mcp-server--handle-tools-call performs.
     (let ((mcp-server--current-client-id  "client-42")
           (mcp-server--current-request-id "req-7")
           (mcp-server-tools-filter nil))
       (mcp-server-tools-call "capture-test" '()))
     (should (equal "client-42" captured-client-id))
     (should (equal "req-7"     captured-request-id)))))

(ert-deftest mcp-test-deferred-context-vars-nil-outside-call ()
  "mcp-server--current-client-id and -request-id default to nil."
  (should (null mcp-server--current-client-id))
  (should (null mcp-server--current-request-id)))

;;; mcp-server-send-tool-result JSON structure

(ert-deftest mcp-test-deferred-send-tool-result-structure ()
  "mcp-server-send-tool-result sends a well-formed JSON-RPC success response."
  (let (sent-json)
    (mcp-test-with-mock
     ((mcp-server-transport-send-raw
       (lambda (_transport _client-id json-str)
         (setq sent-json json-str))))
     (mcp-server-send-tool-result "client-1" "req-1" "the answer")
     (should sent-json)
     (let ((parsed (json-parse-string sent-json :object-type 'alist
                                      :array-type 'array)))
       (should (equal "2.0"   (alist-get 'jsonrpc parsed)))
       (should (equal "req-1" (alist-get 'id      parsed)))
       (let ((result (alist-get 'result parsed)))
         (should result)
         ;; isError must be false
         (should (eq :false (alist-get 'isError result)))
         ;; content must be a vector with one text item
         (let* ((content (alist-get 'content result))
                (item    (and (vectorp content) (aref content 0))))
           (should item)
           (should (equal "text"      (alist-get 'type item)))
           (should (equal "the answer" (alist-get 'text item)))))))))

;;; mcp-server-send-tool-error JSON structure

(ert-deftest mcp-test-deferred-send-tool-error-structure ()
  "mcp-server-send-tool-error sends a well-formed JSON-RPC error response."
  (let (sent-json)
    (mcp-test-with-mock
     ((mcp-server-transport-send-raw
       (lambda (_transport _client-id json-str)
         (setq sent-json json-str))))
     (mcp-server-send-tool-error "client-2" "req-2" "something went wrong")
     (should sent-json)
     (let ((parsed (json-parse-string sent-json :object-type 'alist
                                      :array-type 'array)))
       (should (equal "2.0"   (alist-get 'jsonrpc parsed)))
       (should (equal "req-2" (alist-get 'id      parsed)))
       (let ((result (alist-get 'result parsed)))
         (should result)
         ;; isError must be true
         (should (eq t (alist-get 'isError result)))
         (let* ((content (alist-get 'content result))
                (item    (and (vectorp content) (aref content 0))))
           (should item)
           (should (equal "text"                (alist-get 'type item)))
           (should (equal "something went wrong" (alist-get 'text item)))))))))

;;; Public API functions exist

(ert-deftest mcp-test-deferred-public-api-functions-exist ()
  "The deferred public API functions are defined."
  (should (fboundp 'mcp-server-send-tool-result))
  (should (fboundp 'mcp-server-send-tool-error)))

(ert-deftest mcp-test-deferred-context-vars-defined ()
  "The deferred context variables are defined."
  (should (boundp 'mcp-server--current-client-id))
  (should (boundp 'mcp-server--current-request-id)))

(provide 'test-mcp-server-deferred)
;;; test-mcp-server-deferred.el ends here
