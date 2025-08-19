;;; minimal-test-config.el --- Minimal config for MCP server testing

;; Add the project root directory to load path
(add-to-list 'load-path (expand-file-name "../.." (file-name-directory load-file-name)))

;; Load the MCP server modules
(require 'mcp-server)
(require 'mcp-server-transport)
(require 'mcp-server-transport-unix)
(require 'mcp-server-tools)
(require 'mcp-server-emacs-tools)
(require 'mcp-server-security)

;; Configure for testing
(setq mcp-server-debug t)  ; Enable debug logging
(setq mcp-server-socket-name "test-server")  ; Use predictable socket name
(setq mcp-server-security--prompt-for-permissions nil)  ; Disable prompting for tests

;; Start the server automatically
(mcp-server-start-unix-named "test-server")

;; Print status
(message "MCP Server started with socket name: test-server")
(message "Socket path: %s" (mcp-server-get-socket-path))

;; Keep Emacs running
(while t
  (sleep-for 1))

;;; minimal-test-config.el ends here