# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is an Emacs MCP (Model Context Protocol) Server implementation written in pure Elisp. It enables direct integration between Large Language Models and Emacs internals by exposing Emacs functionality through standardized MCP tools.

## Code Generation

- ALWAYS ensure that parentheses are perfectly balanced!
- Be as concise as possible.
- Be extremely careful with the protocol and transport layers code. Always test code if making changes to it.
- Follow idiomatic Elisp conventions.

## Elisp Code Conventions

### Naming Conventions

**Private symbols use double-dash prefix:**
```elisp
;; Public API
(defun mcp-server-start () ...)
(defvar mcp-server-running nil)

;; Private/internal (not for external use)
(defun mcp-server--handle-message () ...)
(defvar mcp-server--client-counter 0)
```

**Package prefixes are mandatory:**
- All symbols must start with `mcp-server-` (or module-specific like `mcp-server-tools-`)
- This prevents namespace pollution

### Variables: defcustom vs defvar

**Use `defcustom` for user-configurable settings:**
```elisp
(defcustom mcp-server-debug nil
  "Whether to enable debug logging."
  :type 'boolean
  :group 'mcp-server)
```

**Use `defvar` only for internal state:**
```elisp
(defvar mcp-server-running nil
  "Whether the MCP server is currently running.")
```

**All defcustom must include:**
- `:type` specification
- `:group 'mcp-server` (or appropriate subgroup)
- Descriptive docstring

### String Comparisons

**Always use `string=` for string equality:**
```elisp
;; Correct
(when (string= method "initialize") ...)

;; Avoid for strings (works but less explicit)
(when (equal method "initialize") ...)
```

### Error Handling

**Standard pattern uses `condition-case`:**
```elisp
(condition-case err
    (do-something-risky)
  (error
   (handle-error (error-message-string err))))
```

**Use `throw/catch` only for non-local exit (not errors):**
```elisp
;; Used in message handler for early exit after successful send
(catch 'mcp-handled
  (when success
    (throw 'mcp-handled 'success))
  (fallback-action))
```

**Signal errors with `error`:**
```elisp
(unless tool
  (error "Tool not found: %s" name))
```

### JSON Schema Conventions

**Use vectors for `required` fields:**
```elisp
;; Correct - vector
:input-schema '((type . "object")
                (required . ["expression"]))

;; Wrong - list (won't serialize correctly)
:input-schema '((type . "object")
                (required . ("expression")))
```

### JSON Boolean Values

**NEVER use `:json-false` in this codebase. It is banned.**

Always use `t` for JSON `true` and `:false` for JSON `false`:
```elisp
;; Correct
:annotations '((readOnlyHint . t)
               (destructiveHint . :false))

;; BANNED - will break the MCP protocol!
:annotations '((readOnlyHint . t)
               (destructiveHint . :json-false))  ; DO NOT USE
```

Why: `:json-false` is what Emacs returns when *parsing* JSON input, but `json-serialize` only accepts `:false` for output. This codebase produces JSON for the MCP protocol. Using `:json-false` will cause serialization errors and break client communication.

### Autoload Cookies

**Use `;;;###autoload` only for user-facing interactive commands:**
```elisp
;;;###autoload
(defun mcp-server-start () ...)      ; User command - autoload

(defun mcp-server-main () ...)        ; Internal entry point - no autoload
```

### Abstraction Boundaries

**Never call private (`--`) functions from other modules:**
```elisp
;; Wrong - reaching into transport internals
(mcp-server-transport-unix--get-client client-id)

;; Correct - use public API
(mcp-server-transport-send-raw transport-name client-id json-str)
```

### Event Loop Best Practices

**Use `sit-for` instead of `sleep-for` for waiting:**
```elisp
;; Correct - allows event processing
(while running
  (sit-for 0.1))

;; Avoid - blocks event processing
(while running
  (sleep-for 0.1))
```

### Documentation

**Every public function needs a docstring:**
```elisp
(defun mcp-server-tools-call (name arguments)
  "Call tool NAME with ARGUMENTS.
Returns a list of content items in MCP format.
Respects `mcp-server-tools-filter' - disabled tools cannot be called."
  ...)
```

### Struct Definitions

**Use `cl-defstruct` for data structures:**
```elisp
(cl-defstruct mcp-server-tool
  "Structure representing an MCP tool."
  name
  title
  description
  input-schema
  function)
```

### Required Emacs Version

This project requires **Emacs 27.1+** for native JSON support (`json-serialize`, `json-parse-string`).

### Version Bumping

Version bumping is automated via [release-please](https://github.com/googleapis/release-please). Do not edit version strings manually. Write conventional commits (`feat:`, `fix:`, `feat!:` / `BREAKING CHANGE:` for major bumps) on `main`; release-please opens a release PR that updates both version locations in `mcp-server.el`, updates `CHANGELOG.md`, and (on merge) creates a `vX.Y.Z` git tag and GitHub Release.

The two version locations that release-please manages (marked with `x-release-please-version` comments):
1. Header comment: `;; Version: X.Y.Z ;; x-release-please-version`
2. Runtime constant: `(defconst mcp-server-version "X.Y.Z" ; x-release-please-version`

Config lives in `.github/release-please-config.json`, `.github/.release-please-manifest.json`, and `.github/workflows/release-please.yml`.

## Key Architecture Components

### Modular Transport System
The server uses a pluggable transport architecture:
- `mcp-server-transport.el` - Base transport interface
- `mcp-server-transport-unix.el` - Unix domain socket implementation
- `mcp-server-transport-tcp.el` - TCP transport (planned)
- Multiple transport backends can coexist

### Core Protocol Implementation
- `mcp-server.el` - Main entry point and server orchestration
- Full MCP draft specification compliance

### Tool and Security Framework
- `mcp-server-tools.el` - Tool registry and execution framework
- `mcp-server-emacs-tools.el` - Tool loader (loads tools from `tools/` directory)
- `mcp-server-security.el` - Permission management and sandboxing
- `tools/` - Individual tool implementations (self-registering modules)

## Essential Commands

### Development and Testing
- `./test/scripts/test-runner.sh` - Comprehensive test suite for validation
- `./test/scripts/test-runner.sh -v` - Run tests with verbose output  
- `./test/scripts/test-runner.sh -k` - Keep server running for manual testing
- `./test/scripts/test-runner.sh -s` - Test against existing server instance

### Server Management
```elisp
;; Start server with Unix socket (primary transport)
M-x mcp-server-start-unix

;; Start with custom socket name
M-x mcp-server-start-unix-named

;; Configure socket naming strategy
M-x mcp-server-set-socket-name

;; Show server status and connections
M-x mcp-server-status

;; Get current socket path
M-x mcp-server-get-socket-path

;; Stop the server
M-x mcp-server-stop
```

### Testing and Debugging
```elisp
;; Load test configuration
(require 'test-config)

;; Start server with test configuration
M-x mcp-test-start-server

;; Validate refactoring worked correctly
M-x mcp-test-validate-refactoring

;; Toggle debug logging
M-x mcp-server-toggle-debug
```

## Socket Naming Configuration

The server supports multiple socket naming strategies via `mcp-server-socket-name`:

- **Default naming** (`nil`) - Creates `emacs-mcp-server.sock` (recommended for most users)
- **User-based** (`'user`) - Creates `emacs-mcp-server-{username}.sock` (multi-user systems)
- **Session-based** (`'session`) - Creates `emacs-mcp-server-{username}-{pid}.sock` (multiple instances)
- **Custom function** - Dynamic naming via lambda function
- **Custom string** - Fixed naming like `"my-instance"` creates `emacs-mcp-server-my-instance.sock`

## MCP Tool Registry

The server exposes the following tools:

- `eval-elisp` - Execute arbitrary Elisp expressions safely
- `get-diagnostics` - Get flycheck/flymake diagnostics from project buffers

Tools can be selectively enabled via `mcp-server-emacs-tools-enabled`:
```elisp
(setq mcp-server-emacs-tools-enabled 'all)              ; All tools (default)
(setq mcp-server-emacs-tools-enabled '(get-diagnostics)) ; Only diagnostics
```

## Security Model

### Tool Annotations (MCP Specification)

Tools expose behavior hints via annotations that MCP clients use to determine
whether to prompt users for permission:

| Annotation | Description |
|------------|-------------|
| `readOnlyHint` | `true` if tool doesn't modify anything |
| `destructiveHint` | `true` if tool may cause destructive changes |
| `idempotentHint` | `true` if repeated calls have no additional effect |
| `openWorldHint` | `true` if tool interacts with external entities |

Current tool annotations:
- `eval-elisp`: destructive, non-idempotent, open-world (can do anything)
- `get-diagnostics`: read-only, idempotent, closed-world (safe)

### Permission Handling

The security model has two layers:

1. **MCP client prompting** - Clients like Claude Code use tool annotations
   (`destructiveHint`, etc.) to decide whether to prompt users for tool access.

2. **Emacs blocklist** - Dangerous functions (e.g., `delete-file`, `shell-command`)
   are always blocked, regardless of whether the tool was allowed by the client.

By default (`mcp-server-security-prompt-for-permissions` = `nil`):
- Dangerous operations are **blocked silently** (no minibuffer prompt)
- Safe operations are allowed
- The blocklist is always enforced

To enable Emacs-side prompting (approve dangerous operations case-by-case):
```elisp
(setq mcp-server-security-prompt-for-permissions t)
```

This prompts in the minibuffer instead of blocking, letting users approve
dangerous operations individually.

### Permission Caching
- Permission decisions are cached per session
- Comprehensive audit trail of all actions
- View audit log: `M-x mcp-server-security-show-audit-log`

### Input Validation
- JSON Schema validation for all tool inputs
- Protection against code injection attacks
- Sanitization of string inputs and paths

### Execution Sandboxing
- 30-second default timeout for operations
- Memory usage monitoring
- Restricted access to dangerous functions (when Emacs prompting enabled)

## Client Integration Examples

### Claude Desktop Configuration
```json
{
  "mcpServers": {
    "emacs": {
      "command": "/path/to/mcp-wrapper.sh",
      "args": ["~/.emacs.d/.local/cache/emacs-mcp-server.sock"],
      "transport": "stdio"
    }
  }
}
```

### Python Client
```python
from examples.unix_socket_client import EmacsMCPClient

client = EmacsMCPClient()
if client.connect() and client.initialize():
    result = client.call_tool("eval-elisp", {"expression": "(+ 1 2 3)"})
    client.disconnect()
```

### Shell Testing
```bash
# Test full functionality
./test/integration/test-unix-socket-fixed.sh

# Test with custom socket
./test/integration/test-unix-socket-fixed.sh -s /tmp/custom.sock

# Interactive testing
./test/integration/test-unix-socket-fixed.sh -i
```

## Development Workflow

### Adding New Tools

Tools use a **self-registration pattern**: each tool file registers itself at load time via `mcp-server-register-tool`. This eliminates manual registry maintenance.

**Step 1:** Create a new file in `tools/` directory:

```elisp
;;; tools/mcp-server-emacs-tools-my-tool.el
(require 'mcp-server-tools)

(defun mcp-server-emacs-tools--my-tool-handler (args)
  "Handle my-tool invocation with ARGS."
  (let ((param (alist-get 'param args)))
    (format "Result: %s" param)))

;; Self-registration: this runs when the file is loaded
(mcp-server-register-tool
 (make-mcp-server-tool
  :name "my-tool"
  :title "My Tool"
  :description "Description of functionality"
  :input-schema '((type . "object")
                  (properties . ((param . ((type . "string")))))
                  (required . ["param"]))  ; Note: use vector, not list
  :function #'mcp-server-emacs-tools--my-tool-handler))

(provide 'mcp-server-emacs-tools-my-tool)
```

**Step 2:** Register in `mcp-server-emacs-tools.el`:

```elisp
;; Add to mcp-server-emacs-tools--available alist:
(defconst mcp-server-emacs-tools--available
  '((eval-elisp . mcp-server-emacs-tools-eval-elisp)
    (get-diagnostics . mcp-server-emacs-tools-diagnostics)
    (my-tool . mcp-server-emacs-tools-my-tool))  ; Add your tool here
  "Alist mapping tool names (symbols) to their feature names.")
```

### Tool Visibility and Filtering

Tools are filtered at runtime via `mcp-server-tools-filter`. The `mcp-server-emacs-tools` module sets this to check `mcp-server-emacs-tools-enabled`:

- **Disabled tools are invisible** - They don't appear in `tools/list` responses
- **Disabled tools are blocked** - Calling them via `tools/call` returns an error
- **Changes take effect immediately** - No server restart needed

```elisp
;; Enable only specific tools
(setq mcp-server-emacs-tools-enabled '(get-diagnostics))

;; Re-enable all tools
(setq mcp-server-emacs-tools-enabled 'all)
```

### Testing Changes
1. Run `./test/scripts/test-runner.sh` to validate core functionality
2. Test with actual MCP clients using wrapper scripts
3. Verify security controls work as expected
4. Check multi-client concurrent connections

### Debugging Issues
1. Enable debug logging: `M-x mcp-server-toggle-debug`
2. Check server status: `M-x mcp-server-status`
3. List connected clients: `M-x mcp-server-list-clients`
4. View security audit log: `M-x mcp-server-security-show-audit-log`

## File Structure

```
mcp-server/
├── mcp-server.el                    # Main entry point and orchestration
├── mcp-server-transport.el          # Transport interface definition
├── mcp-server-transport-unix.el     # Unix domain socket implementation
├── mcp-server-transport-tcp.el      # TCP transport (planned)
├── mcp-server-tools.el              # Tool registry and execution
├── mcp-server-security.el           # Security and sandboxing
├── mcp-server-emacs-tools.el        # Tool loader (loads from tools/)
├── tools/                           # Individual tool implementations
│   ├── mcp-server-emacs-tools-eval-elisp.el      # eval-elisp tool
│   └── mcp-server-emacs-tools-diagnostics.el     # get-diagnostics tool
├── test/                            # Test suite directory
│   ├── config/                      # Test configuration files
│   ├── fixtures/                    # Test helpers and utilities
│   ├── unit/                        # Unit tests
│   │   ├── test-mcp-emacs-tools.el  # Tool-specific tests
│   │   └── ...
│   ├── scripts/                     # Test runner scripts
│   │   └── test-runner.sh           # Comprehensive test suite
│   └── integration/                 # Integration test scripts
├── mcp-wrapper.py                   # Python wrapper for MCP clients
└── mcp-wrapper.sh                   # Shell wrapper for MCP clients
```

## Multi-Client Architecture

The server supports concurrent connections from multiple MCP clients:
- Each client gets a unique connection ID
- Client state is tracked independently  
- Shared Emacs state requires careful coordination
- Connection cleanup on client disconnect

## Transport Extensibility  

The modular transport design allows adding new transport mechanisms:
- Implement the `mcp-server-transport` interface
- Register with `mcp-server-transport-register`
- Support for stdio, TCP, WebSocket, etc.
- Always update relevant files with significant changes. For example, when changing tests make sure to update the tests/README.md file.
