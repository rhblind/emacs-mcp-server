;;; mcp-server-emacs-tools.el --- Emacs-specific MCP Tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This module loads Emacs-specific MCP tools from the tools/ directory.
;; Tools self-register on require.  Use `mcp-server-emacs-tools-enabled'
;; to control which tools are exposed to LLM clients at runtime.

;;; Code:

(require 'mcp-server-tools)

(defgroup mcp-server-emacs-tools nil
  "Emacs-specific MCP tools configuration."
  :group 'mcp-server
  :prefix "mcp-server-emacs-tools-")

(defcustom mcp-server-emacs-tools-enabled 'all
  "Which MCP tools to enable.
Can be `all' to enable all available tools, or a list of tool
names (symbols) to enable selectively.

Available tools:
- `eval-elisp' - Execute arbitrary Elisp expressions
- `get-diagnostics' - Get flycheck/flymake diagnostics
- `org-agenda' - Agenda/TODO views
- `org-search' - Search org headings
- `org-get-node' - Fetch heading or file contents
- `org-list-templates' - List capture templates
- `org-list-tags' - List tags with usage counts
- `org-capture' - Create a new org entry
- `org-update-node' - Modify an org heading
- `org-refile' - Move a heading
- `org-archive' - Archive a heading
- `org-clock' - Clock in/out/cancel
- `org-roam-search' - Search org-roam nodes (if installed)
- `org-roam-get-node' - Fetch roam node with backlinks (if installed)
- `org-roam-capture' - Create roam node (if installed)

Example: \\='(get-diagnostics org-agenda org-search) to enable only
a subset.

Changes take effect immediately - disabled tools are hidden from
LLM clients and cannot be called."
  :type '(choice (const :tag "All tools" all)
                 (repeat :tag "Selected tools" symbol))
  :group 'mcp-server-emacs-tools)

(defconst mcp-server-emacs-tools--available
  '((eval-elisp . mcp-server-emacs-tools-eval-elisp)
    (get-diagnostics . mcp-server-emacs-tools-diagnostics)
    (org-agenda . mcp-server-emacs-tools-org-agenda)
    (org-search . mcp-server-emacs-tools-org-search)
    (org-get-node . mcp-server-emacs-tools-org-get-node)
    (org-list-templates . mcp-server-emacs-tools-org-list-templates)
    (org-list-tags . mcp-server-emacs-tools-org-list-tags)
    (org-capture . mcp-server-emacs-tools-org-capture)
    (org-update-node . mcp-server-emacs-tools-org-update-node)
    (org-refile . mcp-server-emacs-tools-org-refile)
    (org-archive . mcp-server-emacs-tools-org-archive)
    (org-clock . mcp-server-emacs-tools-org-clock)
    (org-roam-search . mcp-server-emacs-tools-org-roam-search)
    (org-roam-get-node . mcp-server-emacs-tools-org-roam-get-node)
    (org-roam-capture . mcp-server-emacs-tools-org-roam-capture))
  "Alist mapping tool names (symbols) to their feature names.")

;; Add tools directory to load path
(let* ((this-file (or load-file-name buffer-file-name))
       (tools-dir (and this-file
                       (expand-file-name "tools" (file-name-directory this-file)))))
  (when tools-dir
    (add-to-list 'load-path tools-dir)))

(defun mcp-server-emacs-tools--tool-enabled-p (tool-name)
  "Return non-nil if TOOL-NAME is enabled.
TOOL-NAME can be a string or symbol."
  (let ((name-sym (if (stringp tool-name) (intern tool-name) tool-name)))
    (or (eq mcp-server-emacs-tools-enabled 'all)
        (memq name-sym mcp-server-emacs-tools-enabled))))

;; Set up the filter for mcp-server-tools
(setq mcp-server-tools-filter #'mcp-server-emacs-tools--tool-enabled-p)

;; Load all tool modules (they self-register)
(dolist (tool-spec mcp-server-emacs-tools--available)
  (require (cdr tool-spec)))

(provide 'mcp-server-emacs-tools)

;;; mcp-server-emacs-tools.el ends here
