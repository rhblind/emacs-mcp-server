;;; mcp-server-emacs-tools-org-common.el --- Shared helpers for org tools -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Shared helpers for the org-* and org-roam-* MCP tools.  Provides node
;; resolution, serialization, path validation, and a with-node macro.
;; All operations go through named org APIs; no text scraping.

;;; Code:

(require 'org)
(require 'org-id)
(require 'org-element)

(defgroup mcp-server-emacs-tools-org nil
  "Configuration for org-mode MCP tools."
  :group 'mcp-server-emacs-tools
  :prefix "mcp-server-emacs-tools-org-")

(defcustom mcp-server-emacs-tools-org-auto-save t
  "When non-nil, write tools save affected buffers after modification."
  :type 'boolean
  :group 'mcp-server-emacs-tools-org)

(defcustom mcp-server-emacs-tools-org-auto-id t
  "When non-nil, read tools assign IDs to nodes they return."
  :type 'boolean
  :group 'mcp-server-emacs-tools-org)

(defcustom mcp-server-emacs-tools-org-allowed-roots nil
  "Directories in which org tools may read or write files.
If nil, falls back to directories containing `org-directory' and
`org-agenda-files'.  Paths outside these roots are rejected."
  :type '(repeat directory)
  :group 'mcp-server-emacs-tools-org)

(defcustom mcp-server-emacs-tools-org-max-body-bytes 100000
  "Maximum body bytes returned by org-get-node and org-roam-get-node.
Larger bodies are truncated and the response carries a `truncated' flag."
  :type 'integer
  :group 'mcp-server-emacs-tools-org)

(provide 'mcp-server-emacs-tools-org-common)

;;; mcp-server-emacs-tools-org-common.el ends here
