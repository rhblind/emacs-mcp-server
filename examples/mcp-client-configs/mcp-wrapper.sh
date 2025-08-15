#!/bin/bash

# emacs-mcp-wrapper.sh - Shell wrapper for Emacs MCP Server
#
# This script connects MCP clients to the Emacs MCP Server via Unix domain sockets.
# It can be used with Claude Desktop or other MCP-enabled applications.
#
# Usage:
#   ./emacs-mcp-wrapper.sh [SOCKET_NAME]
#
# Environment Variables:
#   EMACS_MCP_SOCKET_NAME: Override socket name (default: "primary")
#   EMACS_MCP_SOCKET_PATH: Full path to socket (overrides discovery)
#   EMACS_MCP_TIMEOUT: Connection timeout (default: 10)
#   EMACS_MCP_DEBUG: Enable debug logging

set -euo pipefail

# Configuration
SOCKET_NAME="${1:-${EMACS_MCP_SOCKET_NAME:-primary}}"
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

# Find socket path
find_socket() {
    # Check explicit path first
    if [[ -n "${EMACS_MCP_SOCKET_PATH:-}" && -S "$EMACS_MCP_SOCKET_PATH" ]]; then
        echo "$EMACS_MCP_SOCKET_PATH"
        return 0
    fi
    
    # Determine base directories based on platform
    local base_dirs=()
    if [[ "$(uname)" == "Darwin" ]]; then
        # macOS: use ~/Library/Caches/emacs-mcp-server
        local cache_dir="$HOME/Library/Caches/emacs-mcp-server"
        base_dirs=("$cache_dir" "${XDG_RUNTIME_DIR:-/tmp}" "/tmp")
    else
        # Linux: use ~/.emacs.d/emacs-mcp-server
        local xdg_dir="$HOME/.emacs.d/emacs-mcp-server"
        base_dirs=("${XDG_RUNTIME_DIR:-$xdg_dir}" "$xdg_dir" "/tmp")
    fi
    
    local username="${USER:-unknown}"
    local socket_candidates=()
    
    # Build candidate paths based on socket name
    case "$SOCKET_NAME" in
        "user")
            for base_dir in "${base_dirs[@]}"; do
                socket_candidates+=("$base_dir/emacs-mcp-server-$username.sock")
            done
            ;;
        "session")
            # Find session-based sockets (most recent)
            for base_dir in "${base_dirs[@]}"; do
                local session_sockets=("$base_dir"/emacs-mcp-server-"$username"-*.sock)
                if [[ ${#session_sockets[@]} -gt 0 && -e "${session_sockets[0]}" ]]; then
                    # Sort by modification time, newest first
                    printf '%s\n' "${session_sockets[@]}" | while IFS= read -r sock; do
                        [[ -S "$sock" ]] && echo "$sock"
                    done | head -1 && return 0
                fi
                socket_candidates+=("$base_dir/emacs-mcp-server-$username.sock")
            done
            ;;
        *)
            # Fixed socket name
            for base_dir in "${base_dirs[@]}"; do
                socket_candidates+=("$base_dir/emacs-mcp-server-$SOCKET_NAME.sock")
            done
            ;;
    esac
    
    # Add fallback candidates
    for base_dir in "${base_dirs[@]}"; do
        socket_candidates+=(
            "$base_dir/emacs-mcp-server-primary.sock"
            "$base_dir/emacs-mcp-server-$username.sock"
        )
    done
    
    # Find first existing socket
    for candidate in "${socket_candidates[@]}"; do
        if [[ -S "$candidate" ]]; then
            echo "$candidate"
            return 0
        fi
    done
    
    # Try PID-based discovery as last resort
    for base_dir in "${base_dirs[@]}"; do
        local pid_sockets=("$base_dir"/emacs-mcp-server-*.sock)
        for socket in "${pid_sockets[@]}"; do
            if [[ -S "$socket" ]]; then
                echo "$socket"
                return 0
            fi
        done
    done
    
    # Return default path even if it doesn't exist (will fail later with clear error)
    echo "/tmp/emacs-mcp-server-$SOCKET_NAME.sock"
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
    
    local socket_path
    socket_path=$(find_socket)
    
    debug "Using socket: $socket_path"
    
    if [[ ! -S "$socket_path" ]]; then
        error "Socket not found: $socket_path"
        error "Make sure Emacs is running with: M-x mcp-server-start-unix"
        error "Or configure socket name with: M-x mcp-server-set-socket-name"
        exit 1
    fi
    
    debug "Connected to Emacs MCP Server"
    
    # Use socat to forward stdin/stdout to Unix socket
    exec socat STDIO "UNIX-CONNECT:$socket_path"
}

# Handle help
if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
    cat << EOF
Usage: $0 [SOCKET_NAME]

Connect MCP clients to Emacs MCP Server via Unix domain sockets.

Arguments:
  SOCKET_NAME    Socket name to connect to (default: "primary")
                 Special values: "user", "session"

Environment Variables:
  EMACS_MCP_SOCKET_NAME    Override socket name
  EMACS_MCP_SOCKET_PATH    Full path to socket (overrides discovery)
  EMACS_MCP_TIMEOUT        Connection timeout (not used in shell version)
  EMACS_MCP_DEBUG          Enable debug logging

Examples:
  $0                       # Connect to primary socket
  $0 user                  # Connect to user-based socket
  $0 my-instance          # Connect to custom named socket
  
  EMACS_MCP_DEBUG=1 $0     # Enable debug output
  
For use with Claude Desktop, add to your config:
{
  "mcpServers": {
    "emacs": {
      "command": "/path/to/emacs-mcp-wrapper.sh",
      "args": ["primary"],
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
    
    # Check platform-specific directories
    search_dirs=()
    if [[ "$(uname)" == "Darwin" ]]; then
        search_dirs=("$HOME/Library/Caches/emacs-mcp-server" "${XDG_RUNTIME_DIR:-/tmp}" "/tmp")
    else
        search_dirs=("$HOME/.emacs.d/emacs-mcp-server" "${XDG_RUNTIME_DIR:-/tmp}" "/tmp")
    fi
    
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