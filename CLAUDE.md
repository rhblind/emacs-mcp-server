# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is an Emacs MCP (Model Context Protocol) Server implementation written in pure Elisp. It enables direct integration between Large Language Models and Emacs internals by exposing Emacs functionality through standardized MCP tools.

## Key Architecture Components

### Modular Transport System
The server uses a pluggable transport architecture:
- `mcp-server-transport.el` - Base transport interface
- `mcp-server-transport-unix.el` - Unix domain socket implementation  
- `mcp-server-transport-tcp.el` - TCP transport (planned)
- Multiple transport backends can coexist

### Core Protocol Implementation
- `mcp-server-protocol.el` - JSON-RPC 2.0 message handling and MCP lifecycle
- `mcp-server.el` - Main entry point and server orchestration
- Full MCP draft specification compliance

### Tool and Security Framework
- `mcp-server-tools.el` - Tool registry and execution framework
- `mcp-server-emacs-tools.el` - Emacs-specific tool implementations
- `mcp-server-security.el` - Permission management and sandboxing

## Essential Commands

### Development and Testing
- `./test-runner.sh` - Comprehensive test suite for validation
- `./test-runner.sh -v` - Run tests with verbose output  
- `./test-runner.sh -k` - Keep server running for manual testing
- `./test-runner.sh -s` - Test against existing server instance

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

- **Fixed naming** (`"primary"`) - Creates `/tmp/mcp-server-primary.sock` (best for MCP clients)
- **User-based** (`'user`) - Creates `/tmp/mcp-server-{username}.sock` (multi-user systems)
- **Session-based** (`'session`) - Creates `/tmp/mcp-server-{username}-{pid}.sock` (multiple instances)
- **Custom function** - Dynamic naming via lambda function
- **Default** (`nil`) - PID-based naming for backward compatibility

## MCP Tool Registry

The server exposes these core tools:

### Elisp Execution
- `eval-elisp` - Execute arbitrary Elisp expressions safely
- `get-variable` / `set-variable` - Access Emacs variables
- `call-command` - Execute interactive commands

### Buffer Operations  
- `get-buffer-content` / `set-buffer-content` - Read/write buffer contents
- `get-buffer-list` - List all open buffers
- `switch-buffer` - Change active buffer
- `get-major-mode` - Get buffer's major mode

### Cursor and Navigation
- `get-point` / `goto-point` - Position management
- `insert-at-point` - Text insertion
- `get-selection` - Selected text and region info

### System Information
- `get-window-configuration` - Window layout details

## Security Model

### Permission System
- Dangerous operations require user confirmation
- Permission decisions are cached per session  
- Comprehensive audit trail of all actions
- Configurable prompting behavior

### Input Validation
- JSON Schema validation for all tool inputs
- Protection against code injection attacks
- Sanitization of string inputs and paths

### Execution Sandboxing
- 30-second default timeout for operations
- Memory usage monitoring
- Restricted access to dangerous functions

## Client Integration Examples

### Claude Desktop Configuration
```json
{
  "mcpServers": {
    "emacs": {
      "command": "/path/to/mcp-wrapper.sh",
      "args": ["primary"],
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
./examples/test-unix-socket-fixed.sh

# Test with custom socket
./examples/test-unix-socket-fixed.sh -s /tmp/custom.sock

# Interactive testing
./examples/test-unix-socket-fixed.sh -i
```

## Development Workflow

### Adding New Tools
```elisp
(mcp-server-tools-register
 "tool-name"
 "Display Title"
 "Description of functionality"
 '((type . "object")
   (properties . ((param . ((type . "string")))))
   (required . ["param"]))
 (lambda (args)
   (let ((param (alist-get 'param args)))
     (format "Result: %s" param))))
```

### Testing Changes
1. Run `./test-runner.sh` to validate core functionality
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
├── mcp-server-protocol.el           # JSON-RPC 2.0 and MCP protocol
├── mcp-server-transport.el          # Transport interface definition
├── mcp-server-transport-unix.el     # Unix domain socket implementation
├── mcp-server-transport-tcp.el      # TCP transport (planned)
├── mcp-server-tools.el              # Tool registry and execution
├── mcp-server-security.el           # Security and sandboxing
├── mcp-server-emacs-tools.el        # Emacs-specific tool implementations
├── test-config.el                   # Test configuration and utilities
├── test-runner.sh                   # Comprehensive test suite
└── examples/                        # Client examples and integrations
    ├── mcp-client-configs/          # MCP client configuration examples
    ├── unix-socket-client.py        # Python client implementation
    ├── mcp-wrapper.sh              # Shell wrapper for MCP clients
    └── test-unix-socket-fixed.sh   # Socket communication test
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