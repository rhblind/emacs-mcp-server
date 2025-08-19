;;; test-mcp-tools.el --- Tests for MCP Tool Registry -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for tool registry, tool registration, input validation,
;; and tool execution framework.

;;; Code:

(require 'ert)
(require 'test-helpers)
(require 'mcp-server-tools)
(require 'json)

;;; Tool Registration Tests

(ert-deftest mcp-test-tools-register-basic ()
  "Test basic tool registration."
  (mcp-test-with-mock-server
   (let ((tool (mcp-server-tools-register
                "test-tool"
                "Test Tool"
                "A tool for testing"
                '((type . "object")
                  (properties . ((input . ((type . "string")))))
                  (required . ["input"]))
                (lambda (args) "test result"))))
     (should (mcp-server-tool-p tool))
     (should (equal (mcp-server-tool-name tool) "test-tool"))
     (should (equal (mcp-server-tool-title tool) "Test Tool"))
     (should (equal (mcp-server-tool-description tool) "A tool for testing"))
     (should (mcp-server-tool-input-schema tool))
     (should (functionp (mcp-server-tool-function tool))))))

(ert-deftest mcp-test-tools-register-with-output-schema ()
  "Test tool registration with output schema."
  (mcp-test-with-mock-server
   (let ((output-schema '((type . "string")))
         (tool (mcp-server-tools-register
                "test-tool-output"
                "Test Tool with Output"
                "A tool for testing output schema"
                '((type . "object"))
                (lambda (args) "test result")
                output-schema)))
     (should (equal (mcp-server-tool-output-schema tool) output-schema)))))

(ert-deftest mcp-test-tools-register-with-annotations ()
  "Test tool registration with annotations."
  (mcp-test-with-mock-server
   (let ((annotations '((deprecated . t) (version . "1.0")))
         (tool (mcp-server-tools-register
                "test-tool-annotations"
                "Test Tool with Annotations"
                "A tool for testing annotations"
                '((type . "object"))
                (lambda (args) "test result")
                nil
                annotations)))
     (should (equal (mcp-server-tool-annotations tool) annotations)))))

(ert-deftest mcp-test-tools-define-macro ()
  "Test tool definition using macro."
  (mcp-test-with-mock-server
   (mcp-server-tools-define
       "macro-tool"
       "Macro Tool"
       "A tool defined using macro"
       '((type . "object")
         (properties . ((value . ((type . "number")))))
         (required . ["value"]))
     (let ((value (alist-get 'value args)))
       (* value 2)))
   (should (mcp-server-tools-exists-p "macro-tool"))
   (let ((tool (mcp-server-tools-get "macro-tool")))
     (should (equal (mcp-server-tool-name tool) "macro-tool"))
     (should (functionp (mcp-server-tool-function tool))))))

;;; Tool Registry Operations Tests

(ert-deftest mcp-test-tools-exists-p ()
  "Test tool existence checking."
  (mcp-test-with-mock-server
   (should-not (mcp-server-tools-exists-p "nonexistent-tool"))
   (mcp-test-register-mock-tool "existing-tool")
   (should (mcp-server-tools-exists-p "existing-tool"))))

(ert-deftest mcp-test-tools-get ()
  "Test tool retrieval."
  (mcp-test-with-mock-server
   (should-not (mcp-server-tools-get "nonexistent-tool"))
   (mcp-test-register-mock-tool "test-tool")
   (let ((tool (mcp-server-tools-get "test-tool")))
     (should tool)
     (should (equal (mcp-server-tool-name tool) "test-tool")))))

(ert-deftest mcp-test-tools-list-empty ()
  "Test tool listing when registry is empty."
  (mcp-test-with-mock-server
   (let ((tools (mcp-server-tools-list)))
     (should (listp tools))
     (should (= (length tools) 0)))))

(ert-deftest mcp-test-tools-list-with-tools ()
  "Test tool listing with registered tools."
  (mcp-test-with-mock-server
   (mcp-test-register-mock-tool "tool1")
   (mcp-test-register-mock-tool "tool2")
   (mcp-test-register-mock-tool "tool3")
   (let ((tools (mcp-server-tools-list)))
     (should (= (length tools) 3))
     (dolist (tool tools)
       (should (alist-get 'name tool))
       (should (alist-get 'title tool))
       (should (alist-get 'description tool))
       (should (alist-get 'inputSchema tool))))))

(ert-deftest mcp-test-tools-unregister ()
  "Test tool unregistration."
  (mcp-test-with-mock-server
   (mcp-test-register-mock-tool "temp-tool")
   (should (mcp-server-tools-exists-p "temp-tool"))
   (mcp-server-tools-unregister "temp-tool")
   (should-not (mcp-server-tools-exists-p "temp-tool"))))

(ert-deftest mcp-test-tools-clear ()
  "Test clearing all tools."
  (mcp-test-with-mock-server
   (mcp-test-register-mock-tool "tool1")
   (mcp-test-register-mock-tool "tool2")
   (should (> (length (mcp-server-tools-list)) 0))
   (mcp-server-tools-clear)
   (should (= (length (mcp-server-tools-list)) 0))))

;;; Tool Execution Tests

(ert-deftest mcp-test-tools-call-basic ()
  "Test basic tool execution."
  (mcp-test-with-mock-server
   (mcp-test-register-mock-tool "echo-tool")
   (let ((result (mcp-server-tools-call "echo-tool" '((input . "hello world")))))
     (should (stringp result))
     (should (string-match-p "hello world" result)))))

(ert-deftest mcp-test-tools-call-nonexistent ()
  "Test calling nonexistent tool."
  (mcp-test-with-mock-server
   (should-error (mcp-server-tools-call "nonexistent-tool" '((input . "test"))))))

(ert-deftest mcp-test-tools-call-with-validation ()
  "Test tool call with input validation."
  (mcp-test-with-mock-server
   (mcp-server-tools-register
    "strict-tool"
    "Strict Tool"
    "Tool with strict input validation"
    '((type . "object")
      (properties . ((number . ((type . "number") (minimum . 0))))
      (required . ["number"]))
    (lambda (args) (format "Got number: %s" (alist-get 'number args))))
   
   (should (mcp-server-tools-call "strict-tool" '((number . 42))))
   (should-error (mcp-server-tools-call "strict-tool" '((number . -1))))
   (should-error (mcp-server-tools-call "strict-tool" '((wrong-param . 42))))))

(ert-deftest mcp-test-tools-call-with-complex-args ()
  "Test tool call with complex arguments."
  (mcp-test-with-mock-server
   (mcp-server-tools-register
    "complex-tool"
    "Complex Tool"
    "Tool with complex input"
    '((type . "object")
      (properties . 
       ((list . ((type . "array") (items . ((type . "string")))))
        (nested . ((type . "object") 
                   (properties . ((key . ((type . "string"))))))))
      (required . ["list" "nested"]))
    (lambda (args)
      (format "List: %s, Nested key: %s"
              (alist-get 'list args)
              (alist-get 'key (alist-get 'nested args)))))
   
   (let ((result (mcp-server-tools-call 
                  "complex-tool" 
                  '((list . ["a" "b" "c"])
                    (nested . ((key . "value")))))))
     (should (string-match-p "List:.*Nested key: value" result)))))

;;; Input Validation Tests

(ert-deftest mcp-test-tools-validate-input-valid ()
  "Test input validation with valid data."
  (let ((schema '((type . "object")
                  (properties . ((name . ((type . "string")))
                                 (age . ((type . "number")))))
                  (required . ["name"])))
        (input '((name . "test") (age . 25))))
    (should (mcp-server-tools--validate-input input schema))))

(ert-deftest mcp-test-tools-validate-input-missing-required ()
  "Test input validation with missing required field."
  (let ((schema '((type . "object")
                  (properties . ((name . ((type . "string")))
                                 (age . ((type . "number")))))
                  (required . ["name"])))
        (input '((age . 25))))
    (should-error (mcp-server-tools--validate-input input schema))))

(ert-deftest mcp-test-tools-validate-input-wrong-type ()
  "Test input validation with wrong type."
  (let ((schema '((type . "object")
                  (properties . ((name . ((type . "string")))
                                 (age . ((type . "number")))))
                  (required . ["name"])))
        (input '((name . "test") (age . "not-a-number"))))
    (should-error (mcp-server-tools--validate-input input schema))))

(ert-deftest mcp-test-tools-validate-input-array ()
  "Test input validation with arrays."
  (let ((schema '((type . "object")
                  (properties . ((items . ((type . "array")
                                            (items . ((type . "string"))))))
                  (required . ["items"])))
        (valid-input '((items . ["a" "b" "c"])))
        (invalid-input '((items . ["a" 123 "c"]))))
    (should (mcp-server-tools--validate-input valid-input schema))
    (should-error (mcp-server-tools--validate-input invalid-input schema))))

;;; Tool Metadata Tests

(ert-deftest mcp-test-tools-list-format ()
  "Test tool list format compliance."
  (mcp-test-with-mock-server
   (mcp-server-tools-register
    "metadata-tool"
    "Metadata Tool"
    "Tool for testing metadata"
    '((type . "object")
      (properties . ((input . ((type . "string"))))))
    (lambda (args) "result")
    '((type . "string"))
    '((version . "1.0") (author . "test")))
   
   (let ((tools (mcp-server-tools-list)))
     (should (= (length tools) 1))
     (let ((tool (car tools)))
       (should (alist-get 'name tool))
       (should (alist-get 'title tool))
       (should (alist-get 'description tool))
       (should (alist-get 'inputSchema tool))
       (should (alist-get 'outputSchema tool))
       (should (alist-get 'annotations tool))))))

(ert-deftest mcp-test-tools-list-without-optional-fields ()
  "Test tool list format without optional fields."
  (mcp-test-with-mock-server
   (mcp-server-tools-register
    "minimal-tool"
    "Minimal Tool"
    "Minimal tool without optional fields"
    '((type . "object"))
    (lambda (args) "result"))
   
   (let ((tools (mcp-server-tools-list)))
     (should (= (length tools) 1))
     (let ((tool (car tools)))
       (should (alist-get 'name tool))
       (should (alist-get 'title tool))
       (should (alist-get 'description tool))
       (should (alist-get 'inputSchema tool))
       (should-not (alist-get 'outputSchema tool))
       (should-not (alist-get 'annotations tool))))))

;;; Error Handling Tests

(ert-deftest mcp-test-tools-call-execution-error ()
  "Test tool call with execution error."
  (mcp-test-with-mock-server
   (mcp-server-tools-register
    "error-tool"
    "Error Tool"
    "Tool that throws an error"
    '((type . "object"))
    (lambda (args) (error "Tool execution failed")))
   
   (should-error (mcp-server-tools-call "error-tool" '()))))

(ert-deftest mcp-test-tools-register-duplicate ()
  "Test registering tool with duplicate name."
  (mcp-test-with-mock-server
   (mcp-test-register-mock-tool "duplicate-tool")
   (let ((original-tool (mcp-server-tools-get "duplicate-tool")))
     (mcp-server-tools-register
      "duplicate-tool"
      "New Title"
      "New description"
      '((type . "object"))
      (lambda (args) "new result"))
     (let ((new-tool (mcp-server-tools-get "duplicate-tool")))
       (should-not (equal (mcp-server-tool-title original-tool)
                          (mcp-server-tool-title new-tool)))))))

;;; Initialization Tests

(ert-deftest mcp-test-tools-initialization ()
  "Test tools system initialization."
  (let ((mcp-server-tools--initialized nil))
    (should-not mcp-server-tools--initialized)
    (mcp-server-tools-initialize)
    (should mcp-server-tools--initialized)))

(ert-deftest mcp-test-tools-initialization-idempotent ()
  "Test that tools initialization is idempotent."
  (let ((mcp-server-tools--initialized nil))
    (mcp-server-tools-initialize)
    (mcp-server-tools-initialize)
    (should mcp-server-tools--initialized)))

;;; Performance Tests

(ert-deftest mcp-test-tools-large-registry ()
  "Test performance with large number of tools."
  (mcp-test-with-mock-server
   (dotimes (i 100)
     (mcp-test-register-mock-tool (format "tool-%d" i)))
   (let ((tools (mcp-server-tools-list)))
     (should (= (length tools) 100)))
   (should (mcp-server-tools-exists-p "tool-50"))
   (should (mcp-server-tools-call "tool-25" '((input . "test"))))))

(provide 'test-mcp-tools)
;;; test-mcp-tools.el ends here