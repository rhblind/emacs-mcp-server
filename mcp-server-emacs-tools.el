;;; mcp-server-emacs-tools.el --- Emacs-specific MCP Tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This module provides Emacs-specific MCP tools.

;;; Code:

(require 'mcp-server-tools)
(require 'mcp-server-security)
(require 'cl-lib)

(defun mcp-server-emacs-tools-register ()
  "Register all Emacs-specific MCP tools."
  
  ;; eval-elisp - Execute arbitrary elisp expressions
  (mcp-server-tools-register
   "eval-elisp"
   "Execute Elisp Expression"
   "Execute arbitrary Elisp code and return the result."
   '((type . "object")
     (properties . ((expression . ((type . "string")
                                   (description . "The Elisp expression to evaluate")))))
     (required . ("expression")))
   (lambda (args)
     (let ((expression (alist-get 'expression args)))
       (condition-case err
           (let ((form (read-from-string expression)))
             (format "%S" (eval (car form))))
         (error (format "Error: %s" (error-message-string err))))))))

(provide 'mcp-server-emacs-tools)

;;; mcp-server-emacs-tools.el ends here