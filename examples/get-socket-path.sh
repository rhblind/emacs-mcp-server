#!/bin/bash

# Simple script to get the MCP server socket path from Emacs

echo "Getting socket path from Emacs..."

# Try to get socket path from running Emacs instance
SOCKET_PATH=$(emacs --batch --eval "
(progn 
  (add-to-list 'load-path \"$(pwd)\")
  (require 'mcp-server-transport-unix)
  (princ (or (mcp-server-transport-unix-socket-path) \"No socket active\"))
)" 2>/dev/null)

if [[ "$SOCKET_PATH" == "No socket active" || -z "$SOCKET_PATH" ]]; then
    echo "No active MCP server found."
    echo "Please start the server first with: M-x mcp-server-start-unix"
    exit 1
else
    echo "Socket path: $SOCKET_PATH"
    if [[ -S "$SOCKET_PATH" ]]; then
        echo "Socket file exists and is valid"
        ls -la "$SOCKET_PATH"
    else
        echo "Socket file not found or invalid"
    fi
fi