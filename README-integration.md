# MCP Server for Emacs - LLM Integration Guide

This guide explains how to use the Emacs MCP Server with LLMs and other clients via Unix domain sockets.

## Quick Start

### 1. Start the Server

In Emacs, run:
```elisp
M-x mcp-server-start-unix
```

This will start the MCP server listening on a Unix domain socket. The socket path depends on your configuration and platform:

**Socket Directory (in order of preference):**
1. `mcp-server-socket-directory` (if explicitly set)
2. `~/.emacs.d/.local/cache/` (Emacs cache directory - primary default)
3. `~/Library/Caches/emacs-mcp-server/` (macOS fallback)
4. `~/.emacs.d/emacs-mcp-server/` (Emacs convention fallback)
5. `/tmp/` (final fallback)

**Socket Names:**
- Default: `emacs-mcp-server-<pid>.sock` (PID-based)
- Fixed naming: `emacs-mcp-server-primary.sock` (if configured)
- User-based: `emacs-mcp-server-<username>.sock`
- Session-based: `emacs-mcp-server-<username>-<pid>.sock`

**Default Socket Location:**
`~/.emacs.d/.local/cache/emacs-mcp-server-primary.sock` (when using fixed naming)

### 2. Get the Socket Path

To find the socket path:
```elisp
M-x mcp-server-get-socket-path
```

### 3. Connect a Client

Use any of the provided client examples to connect and interact with the server. **Note**: All client scripts now require the full socket path as an argument, rather than just a socket name.

## Architecture Overview

The Emacs MCP Server uses a modular transport architecture:

```
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
│   LLM Client    │────│  Unix Socket     │────│  Emacs MCP      │
│   (Python/etc)  │    │  Transport       │    │  Server         │
└─────────────────┘    └──────────────────┘    └─────────────────┘
                                │
                       ┌──────────────────┐
                       │   JSON-RPC 2.0   │
                       │   Protocol       │
                       └──────────────────┘
                                │
                       ┌──────────────────┐
                       │   MCP Tools      │
                       │   (Elisp Funcs)  │
                       └──────────────────┘
```

### Key Components

- **Transport Layer**: Pluggable backends (Unix sockets, TCP planned)
- **Protocol Layer**: JSON-RPC 2.0 message handling (integrated into transport layer)
- **Tool Registry**: Elisp functions exposed as MCP tools
- **Security Layer**: Permission management and input validation
- **Multi-client Support**: Concurrent connections via socket server

## Server Commands

### Starting the Server

```elisp
;; Start with Unix domain socket (recommended)
(mcp-server-start-unix)

;; Start with debug logging
(mcp-server-start-unix t)

;; Start with custom socket path
(mcp-server-start-unix t "/tmp/custom-mcp.sock")

;; Start with specific socket name
(mcp-server-start-unix-named "primary")

;; Start with default transport (Unix)
(mcp-server-start)

;; TCP transport (placeholder - not yet implemented)
(mcp-server-start-tcp)
```

### Socket Naming Configuration

The server supports predictable socket naming for easy MCP client integration:

```elisp
;; Configure socket naming (before starting server)
(setq mcp-server-socket-name "primary")        ; Fixed name
(setq mcp-server-socket-name 'user)            ; Username-based  
(setq mcp-server-socket-name 'session)         ; Session-based
(setq mcp-server-socket-name                   ; Custom function
      (lambda () (format "emacs-%s" (system-name))))

;; Interactive configuration
M-x mcp-server-set-socket-name

;; Show current configuration
M-x mcp-server-show-socket-config

;; Preview socket path without starting
M-x mcp-server-get-predicted-socket-path
```

### Managing the Server

```elisp
;; Stop the server
(mcp-server-stop)

;; Restart the server
(mcp-server-restart)

;; Get server status
(mcp-server-status)

;; List connected clients
(mcp-server-list-clients)

;; Get Unix socket path
(mcp-server-get-socket-path)

;; Disconnect a specific client
(mcp-server-disconnect-client "client-1")

;; Toggle debug logging
(mcp-server-toggle-debug)
```

### Socket Name Management Commands

```elisp
;; Interactive socket name configuration
M-x mcp-server-set-socket-name

;; Start server with specific socket name
M-x mcp-server-start-unix-named

;; Show current socket configuration
M-x mcp-server-show-socket-config

;; Preview socket path that would be used
M-x mcp-server-get-predicted-socket-path
```

## Testing and Validation

### Running Tests

```bash
# Run comprehensive test suite
./test/scripts/test-runner.sh

# Run with verbose output
./test/scripts/test-runner.sh -v

# Keep server running after tests
./test/scripts/test-runner.sh -k

# Test against existing server
./test/scripts/test-runner.sh -s
```

### Test Structure

The test suite is organized in the `test/` directory:
- `test/scripts/` - Test runner scripts and utilities
- `test/config/` - Test configuration files
- `test/integration/` - Integration test scripts

## Available Tools

The server exposes these MCP tools for LLM interaction:

### Core Tools

- **`eval-elisp`**: Execute arbitrary Elisp expressions
  ```json
  {"name": "eval-elisp", "arguments": {"expression": "(+ 1 2 3)"}}
  ```

### Buffer Operations

- **`get-buffer-content`**: Get content of a buffer
- **`set-buffer-content`**: Replace buffer content
- **`get-buffer-list`**: List all open buffers
- **`switch-buffer`**: Change active buffer

### Cursor and Point Management

- **`get-point`**: Get current cursor position
- **`goto-point`**: Move cursor to specific position
- **`insert-at-point`**: Insert text at cursor
- **`get-selection`**: Get currently selected text

### Variables and Commands

- **`get-variable`**: Read Emacs variable values
- **`set-variable`**: Set Emacs variable values
- **`call-command`**: Execute interactive commands
- **`get-major-mode`**: Get buffer's major mode

### System Information

- **`get-window-configuration`**: Get window layout info

## Client Examples

### Python Client

```python
#!/usr/bin/env python3
from examples.mcp_wrapper import EmacsMCPWrapper

# Connect to server using full socket path
socket_path = "~/.emacs.d/.local/cache/emacs-mcp-server-primary.sock"
wrapper = EmacsMCPWrapper(socket_path)

# Test connection
if wrapper._validate_socket():
    print("Socket is available")
    # Use wrapper.run() for actual MCP communication
```

### Shell Script Testing

```bash
# Run full test suite
./test/scripts/test-runner.sh

# Run with verbose output
./test/scripts/test-runner.sh -v

# Keep server running for manual testing
./test/scripts/test-runner.sh -k

# Test Unix socket communication (requires full socket path)
./test/integration/test-unix-socket-fixed.sh ~/.emacs.d/.local/cache/emacs-mcp-server-primary.sock

# Use wrapper scripts with full paths
./examples/mcp-client-configs/mcp-wrapper.sh ~/.emacs.d/.local/cache/emacs-mcp-server-primary.sock

# Python wrapper with full path
python3 ./examples/mcp-client-configs/mcp-wrapper.py ~/.emacs.d/.local/cache/emacs-mcp-server-primary.sock
```

### Raw Socket Communication

```bash
# Using socat directly with full socket path
echo '{"jsonrpc":"2.0","id":1,"method":"tools/list"}' | \
    socat - UNIX-CONNECT:~/.emacs.d/.local/cache/emacs-mcp-server-primary.sock
```

## MCP Protocol Flow

### 1. Initialization

```json
// Client -> Server
{
    "jsonrpc": "2.0",
    "id": 1,
    "method": "initialize",
    "params": {
        "protocolVersion": "2024-11-05",
        "capabilities": {},
        "clientInfo": {"name": "my-client", "version": "1.0.0"}
    }
}

// Server -> Client
{
    "jsonrpc": "2.0",
    "id": 1,
    "result": {
        "protocolVersion": "2024-11-05",
        "capabilities": {
            "tools": {"listChanged": true},
            "resources": {"subscribe": true, "listChanged": true},
            "prompts": {"listChanged": true}
        },
        "serverInfo": {
            "name": "mcp-server",
            "title": "Emacs MCP Server", 
            "version": "0.1.0"
        },
        "instructions": "This server provides direct access to Emacs functionality through MCP tools. Use eval-elisp to execute arbitrary elisp code."
    }
}

// Client -> Server (notification)
{
    "jsonrpc": "2.0",
    "method": "notifications/initialized"
}
```

### 2. Tool Discovery

```json
// Client -> Server
{
    "jsonrpc": "2.0",
    "id": 2,
    "method": "tools/list"
}

// Server -> Client
{
    "jsonrpc": "2.0",
    "id": 2,
    "result": {
        "tools": [
            {
                "name": "eval-elisp",
                "title": "Execute Elisp Expression",
                "description": "Execute arbitrary Elisp code and return the result. Use with caution as this provides full access to Emacs functionality.",
                "inputSchema": {
                    "type": "object",
                    "properties": {
                        "expression": {
                            "type": "string",
                            "description": "The Elisp expression to evaluate"
                        }
                    },
                    "required": ["expression"]
                }
            }
        ]
    }
}
```

### 3. Tool Execution

```json
// Client -> Server
{
    "jsonrpc": "2.0",
    "id": 3,
    "method": "tools/call",
    "params": {
        "name": "eval-elisp",
        "arguments": {"expression": "(buffer-name)"}
    }
}

// Server -> Client
{
    "jsonrpc": "2.0",
    "id": 3,
    "result": {
        "content": [
            {"type": "text", "text": "\"*scratch*\""}
        ],
        "isError": false
    }
}
```

## Security Considerations

### Permission System

The server includes a security layer that:

- Validates all input data
- Checks permissions for dangerous operations
- Sandboxes execution with timeouts
- Logs security events for auditing

### Safe Operations

Most read-only operations are automatically allowed:
- Getting buffer content
- Reading variables
- Listing buffers/tools
- Cursor position queries

### Restricted Operations

Some operations require explicit permission:
- Modifying buffer content
- Setting variables
- Executing commands
- File system operations

### Input Validation

All input is validated to prevent:
- Code injection attacks
- Buffer overflow attempts
- Path traversal attacks
- Malformed JSON-RPC messages

### Security Validation

The codebase has been validated with Semgrep security scanning:
- ✅ **0 security vulnerabilities detected**
- ✅ Comprehensive input validation
- ✅ Safe execution sandboxing
- ✅ Permission-based access control
- ✅ Complete audit logging

## Configuration

### Socket Naming Strategies

Choose the best naming strategy for your use case:

#### Fixed Names (Best for MCP clients)
```elisp
(setq mcp-server-socket-name "primary")
;; Creates: ~/.emacs.d/.local/cache/emacs-mcp-server-primary.sock (default location)
```
**Use when**: Single Emacs instance, MCP client integration

#### User-based Names (Best for multi-user systems)
```elisp
(setq mcp-server-socket-name 'user)
;; Creates: ~/.emacs.d/.local/cache/emacs-mcp-server-{username}.sock (default location)
```
**Use when**: Multiple users on same system

#### Session-based Names (Best for multiple instances)
```elisp
(setq mcp-server-socket-name 'session)
;; Creates: ~/.emacs.d/.local/cache/emacs-mcp-server-{username}-{pid}.sock (default location)
```
**Use when**: Multiple Emacs instances per user

#### Custom Functions (Best for complex setups)
```elisp
(setq mcp-server-socket-name 
      (lambda () (format "emacs-%s" (system-name))))
;; Creates: ~/.emacs.d/.local/cache/emacs-mcp-server-emacs-{hostname}.sock (default location)
```
**Use when**: Network setups, containerized environments

### Socket Configuration Options

```elisp
;; Socket naming
(setq mcp-server-socket-name "primary")

;; Socket directory (default: ~/.emacs.d/.local/cache/)
(setq mcp-server-socket-directory "~/.config/emacs-mcp/")

;; Conflict resolution strategy
(setq mcp-server-socket-conflict-resolution 'warn)  ; warn, error, force, auto
```

### Default Settings

```elisp
;; Default transport
(setq mcp-server-default-transport "unix")

;; Debug logging
(setq mcp-server-debug nil)

;; Server capabilities
(setq mcp-server-capabilities
      '((tools . ((listChanged . t)))
        (resources . ((subscribe . t) (listChanged . t)))
        (prompts . ((listChanged . t)))))
```

### MCP Client Integration Setup

For predictable MCP client integration:

```elisp
;; Recommended configuration for MCP clients
(setq mcp-server-socket-name "primary")
(setq mcp-server-socket-conflict-resolution 'warn)

;; Auto-start with predictable naming
(add-hook 'emacs-startup-hook
          (lambda ()
            (mcp-server-start-unix)))
```

Then configure your MCP client:
```json
{
  "mcpServers": {
    "emacs": {
      "command": "/path/to/mcp-wrapper.sh",
      "args": ["~/.emacs.d/.local/cache/emacs-mcp-server-primary.sock"],
      "transport": "stdio"
    }
  }
}
```

### Custom Socket Path

```elisp
;; Use custom socket location
(mcp-server-start-unix t "/path/to/custom.sock")
```

## Troubleshooting

### Server Won't Start

1. Check if another instance is running:
   ```elisp
   (mcp-server-status)
   ```

2. Verify socket permissions and location:
   ```bash
   # Check common socket locations
   ls -la ~/.emacs.d/.local/cache/emacs-mcp-server-*.sock    # Primary default
   ls -la ~/Library/Caches/emacs-mcp-server/*.sock           # macOS fallback
   ls -la ~/.emacs.d/emacs-mcp-server/*.sock                 # Emacs convention fallback
   ls -la /tmp/emacs-mcp-server-*.sock                       # Final fallback
   ```

3. Check for port conflicts (future TCP support)

### Client Connection Issues

1. Verify socket path:
   ```elisp
   (mcp-server-get-socket-path)
   ```

2. Test socket connectivity:
   ```bash
   # Use actual socket path from mcp-server-get-socket-path
   socat - UNIX-CONNECT:~/.emacs.d/.local/cache/emacs-mcp-server-primary.sock
   ```

3. Check client permissions

### Tool Execution Errors

1. Enable debug logging:
   ```elisp
   (mcp-server-toggle-debug)
   ```

2. Check security permissions
3. Validate JSON-RPC message format
4. Review error messages in `*Messages*` buffer

### Performance Issues

1. Monitor client connections:
   ```elisp
   (mcp-server-list-clients)
   ```

2. Check for resource leaks
3. Optimize tool implementations
4. Consider connection limits

## Integration Examples

### Claude Desktop

Create MCP configuration:

```json
{
  "mcpServers": {
    "emacs": {
      "command": "/path/to/mcp-wrapper.sh",
      "args": ["~/.emacs.d/.local/cache/emacs-mcp-server-primary.sock"],
      "transport": "stdio"
    }
  }
}
```

### Custom LLM Integration

```python
import json
import socket

class EmacsIntegration:
    def __init__(self, socket_path):
        self.socket_path = socket_path
        self.client = EmacsMCPClient(socket_path)
    
    def execute_code(self, code):
        """Execute Elisp code and return result."""
        if not self.client.initialized:
            self.client.connect()
            self.client.initialize()
        
        return self.client.call_tool("eval-elisp", {"expression": code})
    
    def get_context(self):
        """Get current Emacs context for LLM."""
        context = {}
        context['buffer'] = self.client.call_tool("get-buffer-content")
        context['point'] = self.client.call_tool("get-point")
        context['mode'] = self.client.call_tool("get-major-mode")
        return context
```

## Advanced Usage

### Multiple Client Support

The server supports multiple concurrent clients:

```python
# Client 1
client1 = EmacsMCPClient()
client1.connect()

# Client 2  
client2 = EmacsMCPClient()
client2.connect()

# Both can operate simultaneously
```

### Custom Tools

Add your own tools to the server:

```elisp
(mcp-server-tools-register
 "my-custom-tool"
 "My Custom Tool"
 "Description of what it does"
 '((type . "object")
   (properties . ((param . ((type . "string"))))))
 (lambda (args)
   (let ((param (alist-get 'param args)))
     (format "Custom result: %s" param))))
```

### Transport Extensions

The architecture supports adding new transports:

```elisp
;; Future TCP transport
(mcp-server-transport-register
 "tcp"
 (make-mcp-server-transport
  :name "TCP Socket"
  :start-fn #'my-tcp-start
  :stop-fn #'my-tcp-stop
  ;; ... other functions
  ))
```

## Contributing

To extend the MCP server:

1. **Add new tools**: Use `mcp-server-tools-register`
2. **Add transports**: Implement the transport interface
3. **Improve security**: Enhance validation and sandboxing
4. **Add tests**: Extend the test suite
5. **Update docs**: Keep documentation current

## Resources

- [MCP Specification](https://modelcontextprotocol.io/)
- [JSON-RPC 2.0 Specification](https://www.jsonrpc.org/specification)
- [Emacs Lisp Reference](https://www.gnu.org/software/emacs/manual/html_node/elisp/)

---

For more information and updates, see the main project repository.