#!/bin/bash

# emacs-mcp-wrapper.sh - Shell wrapper for Emacs MCP Server
#
# This script connects MCP clients to the Emacs MCP Server via Unix domain sockets.
# It can be used with Claude Desktop or other MCP-enabled applications.
#
# Usage:
#   ./emacs-mcp-wrapper.sh <SOCKET_PATH>
#
# Environment Variables:
#   EMACS_MCP_TIMEOUT: Connection timeout (default: 10)
#   EMACS_MCP_DEBUG: Enable debug logging

set -euo pipefail

# Configuration
SOCKET_PATH="${1:-}"
TIMEOUT="${EMACS_MCP_TIMEOUT:-10}"
DEBUG="${EMACS_MCP_DEBUG:-}"

# Logging functions
debug() {
    if [[ -n "$DEBUG" ]]; then
        echo "[EMACS-MCP-WRAPPER] $*" >&2
    fi
}

error() {
    echo "[EMACS-MCP-WRAPPER ERROR] $*" >&2
}

# Validate socket path
validate_socket() {
    local socket_path="$1"
    
    if [[ -z "$socket_path" ]]; then
        error "Socket path is required"
        return 1
    fi
    
    if [[ ! -S "$socket_path" ]]; then
        error "Socket not found or not a socket: $socket_path"
        error "Make sure Emacs is running with: M-x mcp-server-start-unix"
        return 1
    fi
    
    return 0
}

# Check if socat is available
check_socat() {
    if ! command -v socat &> /dev/null; then
        error "socat is required but not installed"
        error "Install with: brew install socat (macOS) or apt-get install socat (Ubuntu)"
        exit 1
    fi
}

# Main function
main() {
    check_socat
    
    if ! validate_socket "$SOCKET_PATH"; then
        exit 1
    fi
    
    debug "Using socket: $SOCKET_PATH"
    debug "Connected to Emacs MCP Server"
    
    # Use socat to forward stdin/stdout to Unix socket
    exec socat STDIO "UNIX-CONNECT:$SOCKET_PATH"
}

# Handle help
if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
    cat << EOF
Usage: $0 <SOCKET_PATH>

Connect MCP clients to Emacs MCP Server via Unix domain sockets.

Arguments:
  SOCKET_PATH    Full path to the Unix socket file

Environment Variables:
  EMACS_MCP_TIMEOUT        Connection timeout (not used in shell version)
  EMACS_MCP_DEBUG          Enable debug logging

Examples:
  $0 ~/.emacs.d/.local/cache/emacs-mcp-server-primary.sock
  $0 /tmp/emacs-mcp-server-myinstance.sock
  
  EMACS_MCP_DEBUG=1 $0 ~/.emacs.d/.local/cache/emacs-mcp-server-primary.sock
  
For use with Claude Desktop, add to your config:
{
  "mcpServers": {
    "emacs": {
      "command": "/path/to/emacs-mcp-wrapper.sh",
      "args": ["~/.emacs.d/.local/cache/emacs-mcp-server-primary.sock"],
      "transport": "stdio"
    }
  }
}

EOF
    exit 0
fi

# List sockets option
if [[ "${1:-}" == "--list-sockets" ]]; then
    echo "Available Emacs MCP Server sockets:"
    
    # Check common socket directories
    search_dirs=(
        "$HOME/.emacs.d/.local/cache"
        "$HOME/Library/Caches/emacs-mcp-server"
        "$HOME/.emacs.d/emacs-mcp-server"
        "/tmp"
    )
    
    for search_dir in "${search_dirs[@]}"; do
        if [[ -d "$search_dir" ]]; then
            for socket in "$search_dir"/emacs-mcp-server-*.sock; do
                if [[ -S "$socket" ]]; then
                    echo "  $socket"
                fi
            done
        fi
    done
    exit 0
fi

# Run main function
main "$@"