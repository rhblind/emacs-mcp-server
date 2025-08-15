#!/bin/bash

# Debug script to isolate the socket path issue

echo "=== Debug Socket Path ==="
echo "Arguments passed: $*"
echo "Number of arguments: $#"
echo "First argument: $1"
echo "All arguments: $@"
echo

# Check environment
echo "=== Environment ==="
echo "PWD: $PWD"
echo "USER: $USER"
echo "HOME: $HOME"
echo "XDG_RUNTIME_DIR: ${XDG_RUNTIME_DIR:-not set}"
echo

# Test argument parsing
SOCKET_PATH=""
if [[ $# -gt 0 ]]; then
    SOCKET_PATH="$1"
    echo "Socket path set from argument: '$SOCKET_PATH'"
else
    echo "No arguments provided"
fi

# Test socket file
if [[ -n "$SOCKET_PATH" ]]; then
    echo "=== Socket File Test ==="
    echo "Testing socket: $SOCKET_PATH"
    if [[ -e "$SOCKET_PATH" ]]; then
        echo "File exists"
        ls -la "$SOCKET_PATH"
        if [[ -S "$SOCKET_PATH" ]]; then
            echo "File is a socket"
        else
            echo "File is NOT a socket"
        fi
    else
        echo "File does not exist"
    fi
fi

# Test socat connectivity
if [[ -n "$SOCKET_PATH" && -S "$SOCKET_PATH" ]]; then
    echo "=== Testing Socket Connection ==="
    echo "Attempting to connect to socket..."
    
    # Simple connection test
    timeout 2 socat - "UNIX-CONNECT:$SOCKET_PATH" <<< '{"jsonrpc":"2.0","id":1,"method":"tools/list"}' 2>&1 | head -5
fi