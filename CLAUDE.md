# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is an Emacs MCP (Model Context Protocol) Server implementation written in pure Elisp. It enables direct integration between Large Language Models and Emacs internals by exposing Emacs functionality through standardized MCP tools.

## Code generation.

- ALWAYS ensure that parantheses are perfectly balanced!
- Be as concise as possible.
- Be extremely careful with the protocol and transport layers code. Always test code if making changes to it.
- Follow idiomatic ELisp conventions.

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
- `mcp-server-emacs-tools.el` - Emacs-specific tool implementations
- `mcp-server-security.el` - Permission management and sandboxing

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

The server exposes the following tool:

### Elisp Execution
- `eval-elisp` - Execute arbitrary Elisp expressions safely

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
├── mcp-server-emacs-tools.el        # Emacs-specific tool implementations
├── test/                            # Test suite directory
│   ├── config/                      # Test configuration files
│   │   ├── test-config.el           # Main test configuration
│   │   ├── minimal-test-config.el   # Minimal test configuration
│   │   └── test-json-false.el       # JSON serialization tests
│   ├── scripts/                     # Test runner scripts
│   │   ├── test-runner.sh           # Comprehensive test suite
│   │   ├── start-test-server.sh     # Test server startup
│   │   ├── test-hello-world.sh      # Hello world test
│   │   ├── test-hello-world.py      # Python test script
│   │   └── debug-test.py            # Debug test utilities
│   └── integration/                 # Integration test scripts
│       ├── test-unix-socket-fixed.sh # Fixed Unix socket tests
│       ├── test-unix-socket.sh      # Original socket tests
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