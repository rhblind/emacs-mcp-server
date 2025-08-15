#!/bin/bash

# test-hello-world.sh - Send "Hello, world!" message to Emacs via MCP
# This script connects to the Emacs MCP server and prints a message to the *Messages* buffer

set -euo pipefail

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Configuration
SOCKET_PATH=""
TIMEOUT=10

log_info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*"
}

# Discover socket path (simplified version)
discover_socket() {
    local runtime_dir="${XDG_RUNTIME_DIR:-/tmp}"
    local socket_dir="$runtime_dir/emacs-mcp-server"
    
    # Check if the socket directory exists
    if [[ -d "$socket_dir" ]]; then
        shopt -s nullglob
        local sockets=("$socket_dir"/*.sock)
        shopt -u nullglob
        
        if [[ ${#sockets[@]} -gt 0 ]]; then
            for socket in "${sockets[@]}"; do
                if [[ -S "$socket" ]]; then
                    echo "$socket"
                    return 0
                fi
            done
        fi
    fi
    
    # Default fallback (though server likely isn't running)
    echo "$socket_dir/primary.sock"
}

# Send JSON-RPC message and get response
send_message() {
    local message="$1"
    local description="$2"
    
    log_info "Sending: $description"
    
    local response
    response=$(echo "$message" | timeout "$TIMEOUT" socat - "UNIX-CONNECT:$SOCKET_PATH" 2>/dev/null || true)
    
    if [[ -n "$response" ]]; then
        log_success "Response received"
        if command -v jq &> /dev/null; then
            echo "$response" | jq '.'
        else
            echo "$response"
        fi
        echo
        return 0
    else
        log_error "No response received or connection failed"
        return 1
    fi
}

# Initialize MCP connection
initialize() {
    local init_message='{
        "jsonrpc": "2.0",
        "id": 1,
        "method": "initialize",
        "params": {
            "protocolVersion": "draft",
            "capabilities": {},
            "clientInfo": {
                "name": "hello-world-test",
                "version": "1.0.0"
            }
        }
    }'
    
    send_message "$init_message" "Initialization request"
}

# Send initialized notification
send_initialized() {
    local initialized_message='{
        "jsonrpc": "2.0",
        "method": "notifications/initialized"
    }'
    
    log_info "Sending: Initialized notification"
    echo "$initialized_message" | socat - "UNIX-CONNECT:$SOCKET_PATH" 2>/dev/null || true
    log_success "Initialized notification sent"
    echo
}

# Send "Hello, world!" message to Emacs *Messages* buffer
send_hello_world() {
    local hello_message='{
        "jsonrpc": "2.0",
        "id": 2,
        "method": "tools/call",
        "params": {
            "name": "eval-elisp",
            "arguments": {
                "expression": "(message \"Hello, world! This message was sent via MCP at %s\" (current-time-string))"
            }
        }
    }'
    
    send_message "$hello_message" "Hello, world! message"
}

# Main function
main() {
    # Check dependencies
    if ! command -v socat &> /dev/null; then
        log_error "socat is required but not installed"
        log_info "Install with: brew install socat (macOS) or apt-get install socat (Ubuntu)"
        exit 1
    fi
    
    # Set socket path if not specified
    if [[ -z "$SOCKET_PATH" ]]; then
        SOCKET_PATH=$(discover_socket)
    fi
    
    log_info "Emacs MCP Hello World Test"
    log_info "Socket path: $SOCKET_PATH"
    echo
    
    # Check if socket exists
    if [[ ! -S "$SOCKET_PATH" ]]; then
        log_error "Socket not found at: $SOCKET_PATH"
        log_info "Make sure Emacs MCP Server is running with: M-x mcp-server-start-unix"
        exit 1
    fi
    
    log_success "Found socket at: $SOCKET_PATH"
    echo
    
    # Run the test sequence
    initialize || exit 1
    sleep 0.5
    
    send_initialized || exit 1
    sleep 0.5
    
    send_hello_world || exit 1
    
    log_success "Hello, world! message sent to Emacs *Messages* buffer!"
    log_info "Check your Emacs *Messages* buffer to see the greeting."
}

# Parse command line arguments
if [[ $# -gt 0 ]]; then
    SOCKET_PATH="$1"
fi

# Run the test
main