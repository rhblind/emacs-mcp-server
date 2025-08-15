# MCP Client Integration Examples

This directory contains examples and tools for integrating various MCP clients with the MCP Server for Emacs.

## Quick Start

1. **Configure Emacs** with predictable socket naming:
   ```elisp
   ;; In your Emacs config
   (setq mcp-server-socket-name "primary")
   
   ;; Start the server
   M-x mcp-server-start-unix
   ```

2. **Configure your MCP client** using one of the examples below.

## Socket Naming Strategies

The Emacs MCP Server supports several socket naming strategies:

### Fixed Name (Recommended for MCP clients)
```elisp
(setq mcp-server-socket-name "primary")
```
Creates: `/tmp/emacs-mcp-server-primary.sock` (or platform-specific directory)

### User-based (Good for multi-user systems)
```elisp
(setq mcp-server-socket-name 'user)
```
Creates: `/tmp/emacs-mcp-server-{username}.sock` (or platform-specific directory)

### Custom Name
```elisp
(setq mcp-server-socket-name "my-instance")
```
Creates: `/tmp/emacs-mcp-server-my-instance.sock` (or platform-specific directory)

## Client Configurations

### Claude Desktop

Add to your Claude Desktop configuration file:

#### Using Shell Wrapper (Recommended)
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

#### Using Python Wrapper
```json
{
  "mcpServers": {
    "emacs": {
      "command": "python3",
      "args": ["/path/to/mcp-wrapper.py", "--socket-name", "primary"],
      "transport": "stdio"
    }
  }
}
```

#### Direct socat (Simple but less robust)
```json
{
  "mcpServers": {
    "emacs": {
      "command": "socat",
      "args": ["-", "UNIX-CONNECT:/tmp/emacs-mcp-server-primary.sock"],
      "transport": "stdio"
    }
  }
}
```

### Using with uvx/pipx

1. **Install the Python wrapper**:
   ```bash
   # Copy wrapper to a directory in PATH
   cp mcp-wrapper.py ~/.local/bin/mcp-server
   chmod +x ~/.local/bin/mcp-server
   ```

2. **Use with Claude CLI**:
   ```bash
   claude add mcp -- mcp-server --socket-name primary
   claude add mcp emacs-mcp -- socat - UNIX-CONNECT:~/Library/Caches/emacs-mcp-server/emacs-mcp-server-claude.sock
   ```

### Custom MCP Client

For your own MCP client implementation:

```python
import socket
import json

# Connect to Emacs MCP Server
sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
sock.connect("/tmp/emacs-mcp-server-primary.sock")

# Send initialization
init_msg = {
    "jsonrpc": "2.0",
    "id": 1,
    "method": "initialize",
    "params": {
        "protocolVersion": "draft",
        "capabilities": {},
        "clientInfo": {"name": "my-client", "version": "1.0.0"}
    }
}

sock.send((json.dumps(init_msg) + "\n").encode())
response = sock.recv(4096).decode()
print(response)
```

## Wrapper Scripts

### Shell Wrapper (`mcp-wrapper.sh`)
- Lightweight shell script using socat
- Automatic socket discovery
- Good for simple integrations
- Handles cleanup on exit

**Usage:**
```bash
./mcp-wrapper.sh primary
./mcp-wrapper.sh user
EMACS_MCP_DEBUG=1 ./mcp-wrapper.sh
```

### Python Wrapper (`mcp-wrapper.py`)
- More robust error handling
- Better cross-platform support
- Bidirectional streaming
- Advanced socket discovery

**Usage:**
```bash
./mcp-wrapper.py --socket-name primary
./mcp-wrapper.py --list-sockets
EMACS_MCP_DEBUG=1 ./mcp-wrapper.py
```

## Environment Variables

Both wrappers support these environment variables:

- `EMACS_MCP_SOCKET_NAME`: Override socket name (default: "primary")
- `EMACS_MCP_SOCKET_PATH`: Full path to socket (overrides discovery)
- `EMACS_MCP_TIMEOUT`: Connection timeout in seconds (default: 10)
- `EMACS_MCP_DEBUG`: Enable debug logging

## Troubleshooting

### Socket Not Found
1. **Check if Emacs MCP Server is running**:
   ```elisp
   M-x mcp-server-status
   ```

2. **Verify socket path**:
   ```elisp
   M-x mcp-server-get-socket-path
   ```

3. **List available sockets**:
   ```bash
   ./mcp-wrapper.sh --list-sockets
   ```

### Connection Refused
1. **Check socket permissions**:
   ```bash
   ls -la /tmp/emacs-mcp-server-*.sock
   ```

2. **Test socket connectivity**:
   ```bash
   echo '{"jsonrpc":"2.0","id":1,"method":"tools/list"}' | socat - UNIX-CONNECT:/tmp/emacs-mcp-server-primary.sock
   ```

### MCP Client Issues
1. **Verify client configuration**: Check JSON syntax and file paths
2. **Check client logs**: Most MCP clients provide debug logging
3. **Test wrapper independently**: Run wrapper script manually to verify connection

## Advanced Configuration

### Multiple Emacs Instances
```elisp
;; Instance 1
(setq mcp-server-socket-name "instance-1")
(mcp-server-start-unix)

;; Instance 2  
(setq mcp-server-socket-name "instance-2")
(mcp-server-start-unix)
```

### Custom Socket Directory
```elisp
(setq mcp-server-socket-directory "~/.config/emacs-mcp/")
(setq mcp-server-socket-name "primary")
```

### Dynamic Socket Naming
```elisp
(setq mcp-server-socket-name 
      (lambda () 
        (format "emacs-%s-%d" (system-name) (emacs-pid))))
```

## Security Considerations

1. **Socket permissions**: Sockets are created with user-only access
2. **Socket location**: Use XDG_RUNTIME_DIR when available for better security
3. **Input validation**: The server validates all MCP requests
4. **Sandboxing**: Dangerous operations require user confirmation

## Performance Tips

1. **Use predictable naming**: Avoids socket discovery overhead
2. **Keep connections alive**: Reuse connections when possible  
3. **Monitor clients**: Use `M-x mcp-server-list-clients`
4. **Clean up**: Stop unused servers to free resources
