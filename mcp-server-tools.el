;;; mcp-server-tools.el --- MCP Tool Registry and Execution -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This module provides the tool registry and execution framework for the
;; MCP server. It handles tool registration, input validation, and safe
;; execution of elisp functions as MCP tools.

;;; Code:

(require 'json)
(require 'cl-lib)

;;; Variables

(defvar mcp-server-tools--registry (make-hash-table :test 'equal)
  "Hash table storing registered MCP tools.")

(defvar mcp-server-tools--initialized nil
  "Whether the tools system has been initialized.")

(defvar mcp-server-tools-filter nil
  "Predicate function to filter which tools are exposed.
When non-nil, should be a function that takes a tool name (string)
and returns non-nil if the tool should be included in listings and
available for execution.  When nil, all registered tools are exposed.")

;;; Tool Definition Structure

(cl-defstruct mcp-server-tool
  "Structure representing an MCP tool."
  name
  title
  description
  input-schema
  output-schema
  function
  annotations)

;;; Tool Registration

(defun mcp-server-register-tool (tool)
  "Register TOOL, an `mcp-server-tool' struct.
This is the preferred declarative interface for tool registration.

Example:
  (mcp-server-register-tool
   (make-mcp-server-tool
    :name \"my-tool\"
    :title \"My Tool\"
    :description \"Does something useful.\"
    :input-schema \\='((type . \"object\") ...)
    :function #\\='my-tool-handler))"
  (unless (mcp-server-tool-p tool)
    (error "Expected mcp-server-tool struct, got %s" (type-of tool)))
  (unless (mcp-server-tool-name tool)
    (error "Tool must have a name"))
  (puthash (mcp-server-tool-name tool) tool mcp-server-tools--registry)
  tool)

(defun mcp-server-tools-register (name title description input-schema function &optional output-schema annotations)
  "Register a new MCP tool (legacy interface).
Prefer `mcp-server-register-tool' for new tools.

NAME is the unique tool identifier.
TITLE is the human-readable display name.
DESCRIPTION explains what the tool does.
INPUT-SCHEMA is a JSON Schema for validating inputs.
FUNCTION is the elisp function to execute.
OUTPUT-SCHEMA is an optional JSON Schema for outputs.
ANNOTATIONS provide additional metadata."
  (mcp-server-register-tool
   (make-mcp-server-tool
    :name name
    :title title
    :description description
    :input-schema input-schema
    :output-schema output-schema
    :function function
    :annotations annotations)))

(defmacro mcp-server-tools-define (name title description input-schema &rest body)
  "Define a new MCP tool with NAME, TITLE, DESCRIPTION, INPUT-SCHEMA and BODY.
This is a convenience macro for registering tools."
  (declare (indent 4) (doc-string 3))
  `(mcp-server-tools-register
    ,name
    ,title
    ,description
    ,input-schema
    (lambda (args)
      ,@body)))

;;; Tool Listing

(defun mcp-server-tools--enabled-p (name)
  "Return non-nil if tool NAME is enabled.
A tool is enabled if `mcp-server-tools-filter' is nil or returns
non-nil for NAME."
  (or (null mcp-server-tools-filter)
      (funcall mcp-server-tools-filter name)))

(defun mcp-server-tools-list ()
  "Return a list of all enabled tools in MCP format.
Tools are filtered by `mcp-server-tools-filter' if set."
  (let ((tools '()))
    (maphash
     (lambda (name tool)
       (when (mcp-server-tools--enabled-p name)
         (push `((name . ,name)
                 (title . ,(mcp-server-tool-title tool))
                 (description . ,(mcp-server-tool-description tool))
                 (inputSchema . ,(mcp-server-tool-input-schema tool))
                 ,@(when (mcp-server-tool-output-schema tool)
                     `((outputSchema . ,(mcp-server-tool-output-schema tool))))
                 ,@(when (mcp-server-tool-annotations tool)
                     `((annotations . ,(mcp-server-tool-annotations tool)))))
               tools)))
     mcp-server-tools--registry)
    (nreverse tools)))

(defun mcp-server-tools-get (name)
  "Get tool by NAME from the registry."
  (gethash name mcp-server-tools--registry))

(defun mcp-server-tools-exists-p (name)
  "Check if tool with NAME exists in the registry."
  (not (null (gethash name mcp-server-tools--registry))))

;;; Input Validation

(defun mcp-server-tools--validate-input (input schema)
  "Validate INPUT against JSON SCHEMA.
Provides basic type checking and required property validation."
  ;; Basic type checking implementation
  ;; Future enhancement: full JSON Schema validation library
  (let ((type (alist-get 'type schema))
        (properties (alist-get 'properties schema))
        (required (alist-get 'required schema)))
    
    ;; Check top-level type
    (cond
     ((string= type "object")
      (unless (listp input)
        (error "Expected object, got %s" (type-of input)))
      
      ;; Check required properties
      (when required
        (dolist (prop required)
          (unless (alist-get (intern prop) input)
            (error "Missing required property: %s" prop))))
      
      ;; Validate property types (simplified)
      (when properties
        (dolist (prop-spec properties)
          (let ((prop-name (car prop-spec))
                (prop-schema (cdr prop-spec))
                (prop-value (alist-get (intern (symbol-name prop-name)) input)))
            (when prop-value
              (mcp-server-tools--validate-property-type prop-value prop-schema))))))
     
     ((string= type "string")
      (unless (stringp input)
        (error "Expected string, got %s" (type-of input))))
     
     ((string= type "number")
      (unless (numberp input)
        (error "Expected number, got %s" (type-of input))))
     
     ((string= type "boolean")
      (unless (booleanp input)
        (error "Expected boolean, got %s" (type-of input))))
     
     ((string= type "array")
      (unless (vectorp input)
        (error "Expected array, got %s" (type-of input)))))
    
    input))

(defun mcp-server-tools--validate-property-type (value schema)
  "Validate VALUE against property SCHEMA."
  (let ((type (alist-get 'type schema)))
    (cond
     ((string= type "string")
      (unless (stringp value)
        (error "Property must be string, got %s" (type-of value))))
     
     ((string= type "number")
      (unless (numberp value)
        (error "Property must be number, got %s" (type-of value))))
     
     ((string= type "boolean")
      (unless (booleanp value)
        (error "Property must be boolean, got %s" (type-of value))))
     
     ((string= type "array")
      (unless (vectorp value)
        (error "Property must be array, got %s" (type-of value)))))))

;;; Tool Execution

(defun mcp-server-tools-call (name arguments)
  "Call tool NAME with ARGUMENTS.
Returns a vector of content items in MCP format, or the symbol
`mcp-deferred' if the tool will send its response asynchronously.
Respects `mcp-server-tools-filter' - disabled tools cannot be called."
  (let ((tool (mcp-server-tools-get name)))
    (unless tool
      (error "Tool not found: %s" name))
    (unless (mcp-server-tools--enabled-p name)
      (error "Tool is disabled: %s" name))

    (condition-case err
        (let ((result (funcall (mcp-server-tool-function tool) arguments)))
          ;; A deferred tool sends its own response asynchronously.
          (if (eq result 'mcp-deferred)
              'mcp-deferred
            (mcp-server-tools--format-result result)))
      (error
       (vector `((type . "text")
                 (text . ,(format "Tool execution failed: %s" (error-message-string err)))))))))

(defun mcp-server-tools--format-result (result)
  "Format RESULT into MCP content format."
  (cond
   ;; If result is already in MCP format (vector of content items)
   ((vectorp result)
    result)
   
   ;; If result is a single content item
   ((and (listp result) (alist-get 'type result))
    (vector result))
   
   ;; If result is a string, wrap it as text content
   ((stringp result)
    (vector `((type . "text")
              (text . ,result))))
   
   ;; If result is any other value, convert to string
   (t
    (vector `((type . "text")
              (text . ,(format "%S" result)))))))

;;; Initialization and Cleanup

(defun mcp-server-tools-init ()
  "Initialize the tools system.
Tools registered before this call are preserved."
  (setq mcp-server-tools--initialized t))

(defun mcp-server-tools-cleanup ()
  "Clean up the tools system.
Tool definitions are preserved since they self-register on require."
  (setq mcp-server-tools--initialized nil))

(defun mcp-server-tools-clear ()
  "Clear all registered tools."
  (clrhash mcp-server-tools--registry))

;;; Utility Functions

(defun mcp-server-tools-count ()
  "Return the number of registered tools."
  (hash-table-count mcp-server-tools--registry))

(defun mcp-server-tools-list-names ()
  "Return a list of all registered tool names."
  (hash-table-keys mcp-server-tools--registry))

(provide 'mcp-server-tools)

;;; mcp-server-tools.el ends here