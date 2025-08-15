# MCP Server for Emacs

A pure Elisp implementation of the Model Context Protocol (MCP) server that enables direct integration between Large Language Models and Emacs internals.

## Overview

This MCP server exposes Emacs functionality through standardized MCP tools, allowing LLMs like Claude to:

- Execute arbitrary elisp code safely
- Read and modify buffer contents
- Navigate and manipulate cursor position
- Access Emacs variables and state
- Execute interactive commands
- Manage windows and frames
- And much more!

## Features

- **Full MCP Protocol Compliance**: Implements MCP draft specification
- **Safe Execution**: Sandboxed elisp evaluation with permission controls
- **Comprehensive Tools**: 12+ built-in tools covering core Emacs functionality
- **Security First**: Input validation, audit logging, and permission management
- **stdio Transport**: JSON-RPC 2.0 over stdin/stdout for easy integration

## Installation

1. Place the files in your Emacs configuration directory:

   ```bash
   # If using Doom Emacs
   cp -r mcp-server ~/.doom.d/lisp/
   ```

2. Add to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "~/.doom.d/lisp/mcp-server")
   (require 'mcp-server)
   ```

## Usage

### Interactive Mode

Start the server interactively within Emacs:

```elisp
M-x mcp-server-start
```

### Subprocess Mode

Run as a subprocess for MCP client integration:

```bash
emacs --batch -l ~/.doom.d/lisp/mcp-server/mcp-server.el -f mcp-server-main
```

### With Claude Code

Configure claude-code to use this server by adding it to your MCP configuration.

```shell
{
"mcpServers": {
    "emacs": {
    "command": "socat",
    "args": ["STDIO", "UNIX-CONNECT:~/Library/Caches/emacs-mcp-server/emacs-mcp-server-claude.sock"],
    "transport": "stdio"
    }
}
}
```

## Available Tools

### Core Elisp Tools

- **`eval-elisp`** - Execute arbitrary elisp expressions
- **`get-variable`** - Read Emacs variable values
- **`set-variable`** - Set Emacs variable values
- **`call-command`** - Execute interactive Emacs commands

### Buffer Operations

- **`get-buffer-content`** - Read buffer contents
- **`set-buffer-content`** - Replace buffer contents
- **`get-buffer-list`** - List all open buffers
- **`switch-buffer`** - Change active buffer
- **`get-major-mode`** - Get buffer's major mode

### Cursor and Selection

- **`get-point`** - Get cursor position information
- **`goto-point`** - Move cursor to specific position
- **`insert-at-point`** - Insert text at cursor
- **`get-selection`** - Get selected text and region info

### Window Management

- **`get-window-configuration`** - Get window layout information

## Security

The server implements multiple security layers:

### Permission System

- Prompts for dangerous operations
- Caches permission decisions
- Audit trail of all actions

### Input Validation

- JSON Schema validation for tool inputs
- Sanitization of string inputs
- Protection against code injection

### Execution Sandboxing

- Timeout protection (30 seconds default)
- Memory usage monitoring
- Restricted function access

### Dangerous Functions

The following functions require explicit permission:

- File system operations (`delete-file`, `write-region`)
- Process execution (`shell-command`, `call-process`)
- System functions (`kill-emacs`, `server-start`)

## Configuration

### Enable/Disable Permission Prompts

```elisp
(mcp-server-security-set-prompting nil)  ; Disable prompts (deny by default)
(mcp-server-security-set-prompting t)    ; Enable prompts (default)
```

### Grant/Deny Permissions

```elisp
(mcp-server-security-grant-permission 'delete-file)
(mcp-server-security-deny-permission 'shell-command)
```

### Add Custom Dangerous Functions

```elisp
(mcp-server-security-add-dangerous-function 'my-dangerous-function)
```

## Examples

### Basic Tool Usage

Execute elisp code:

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": {
    "name": "eval-elisp",
    "arguments": {
      "expression": "(+ 1 2 3)"
    }
  }
}
```

Read buffer contents:

```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "tools/call",
  "params": {
    "name": "get-buffer-content",
    "arguments": {
      "buffer": "*scratch*"
    }
  }
}
```

Move cursor:

```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "method": "tools/call",
  "params": {
    "name": "goto-point",
    "arguments": {
      "position": 100
    }
  }
}
```

## Development

### File Structure

```
mcp-server/
├── mcp-server.el              # Main entry point
├── mcp-server-protocol.el     # MCP protocol implementation
├── mcp-server-tools.el        # Tool registry and execution
├── mcp-server-security.el     # Security and sandboxing
├── mcp-server-emacs-tools.el  # Emacs-specific tools
└── README.md                        # This file
```

### Adding New Tools

Register a new tool:

```elisp
(mcp-server-tools-register
 "my-tool"
 "My Custom Tool"
 "Description of what it does"
 '((type . "object")
   (properties . ((param . ((type . "string")))))
   (required . ["param"]))
 (lambda (args)
   (let ((param (alist-get 'param args)))
     (format "Result: %s" param))))
```

### Testing

Start the server and test with a simple MCP client:

```elisp
;; Start server
(mcp-server-start t)  ; t enables debug logging

;; Send test message
(mcp-server-protocol-process-input
 "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\"}\n")
```

## Troubleshooting

### Enable Debug Logging

```elisp
(mcp-server-toggle-debug)
```

### Check Server Status

```elisp
(mcp-server-status)
```

### View Security Audit Log

```elisp
M-x mcp-server-security-show-audit-log
```

### View Cached Permissions

```elisp
M-x mcp-server-security-show-permissions
```

## Contributing

1. Follow the existing code style and patterns
2. Add appropriate security checks for new tools
3. Include JSON schemas for tool inputs
4. Update documentation for new features
5. Test with actual MCP clients

## License

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

## Architecture

The server is built with a modular architecture:

- **Protocol Layer**: Handles JSON-RPC 2.0 and MCP lifecycle
- **Tool System**: Manages tool registration and execution
- **Security Layer**: Provides sandboxing and permission control
- **Emacs Interface**: Exposes Emacs functionality as tools

This design allows for easy extension and maintenance while ensuring security and reliability.
