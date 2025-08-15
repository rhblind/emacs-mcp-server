#!/usr/bin/env python3
"""
Hello World Test for Emacs MCP Server

This script connects to the Emacs MCP Server via Unix domain sockets
and sends a "Hello, world!" message to the *Messages* buffer.

Usage:
    python test-hello-world.py [socket_path]

If socket_path is not provided, it will attempt to discover the socket automatically.
"""

import socket
import json
import sys
import os
import glob
import stat
from typing import Optional


def discover_socket() -> str:
    """Attempt to discover the Unix socket path automatically."""
    runtime_dir = os.getenv('XDG_RUNTIME_DIR', '/tmp')
    socket_dir = os.path.join(runtime_dir, 'emacs-mcp-server')
    
    # Check if the socket directory exists
    if os.path.isdir(socket_dir):
        # Look for any .sock files in the directory
        search_pattern = os.path.join(socket_dir, "*.sock")
        sockets = glob.glob(search_pattern)
        
        if sockets:
            # Filter to only actual socket files
            valid_sockets = [s for s in sockets if is_socket(s)]
            if valid_sockets:
                # Return the most recently modified socket
                latest_socket = max(valid_sockets, key=os.path.getmtime)
                print(f"Found socket: {latest_socket}")
                return latest_socket
    
    print(f"No existing sockets found in {socket_dir}, using default")
    # Default fallback
    return os.path.join(socket_dir, "primary.sock")


def is_socket(path: str) -> bool:
    """Check if path is a valid socket file."""
    try:
        return os.path.exists(path) and stat.S_ISSOCK(os.stat(path).st_mode)
    except (OSError, AttributeError):
        return False


def send_mcp_message(sock: socket.socket, message: dict) -> Optional[dict]:
    """Send a JSON-RPC message and receive the response."""
    try:
        json_msg = json.dumps(message) + "\n"
        sock.send(json_msg.encode('utf-8'))
        
        # Read until we get a complete line
        buffer = ""
        while "\n" not in buffer:
            chunk = sock.recv(4096).decode('utf-8')
            if not chunk:
                return None
            buffer += chunk
        
        # Extract the first complete message
        line, remaining = buffer.split("\n", 1)
        if line.strip():
            return json.loads(line.strip())
    except Exception as e:
        print(f"Error in message exchange: {e}")
    
    return None


def main():
    # Get socket path from command line or auto-discover
    socket_path = sys.argv[1] if len(sys.argv) > 1 else discover_socket()
    
    print(f"Emacs MCP Hello World Test")
    print(f"Socket path: {socket_path}")
    print()
    
    # Check if socket exists
    if not is_socket(socket_path):
        print(f"Error: Socket not found at {socket_path}")
        print("Make sure Emacs MCP Server is running with: M-x mcp-server-start-unix")
        sys.exit(1)
    
    print(f"Found socket at: {socket_path}")
    print()
    
    try:
        # Connect to socket
        sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        sock.connect(socket_path)
        print("Connected to Emacs MCP Server")
        
        # Initialize
        print("Initializing MCP session...")
        init_message = {
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
        }
        
        response = send_mcp_message(sock, init_message)
        if not response or "result" not in response:
            print("Initialization failed")
            sys.exit(1)
        
        print("Initialization successful")
        
        # Send initialized notification
        initialized_message = {
            "jsonrpc": "2.0",
            "method": "notifications/initialized"
        }
        sock.send((json.dumps(initialized_message) + "\n").encode('utf-8'))
        print("Initialized notification sent")
        
        # Send "Hello, world!" message
        print("Sending Hello, world! message...")
        hello_message = {
            "jsonrpc": "2.0",
            "id": 2,
            "method": "tools/call",
            "params": {
                "name": "eval-elisp",
                "arguments": {
                    "expression": '(message "Hello, world! This message was sent via MCP at %s" (current-time-string))'
                }
            }
        }
        
        response = send_mcp_message(sock, hello_message)
        if response and "result" in response:
            print("SUCCESS: Hello, world! message sent to Emacs *Messages* buffer!")
            print("Check your Emacs *Messages* buffer to see the greeting.")
            
            # Print the response content if available
            content = response["result"].get("content", [])
            for item in content:
                if item.get("type") == "text":
                    print(f"Elisp result: {item.get('text', '')}")
        else:
            error = response.get("error", {}) if response else {}
            print(f"Error sending message: {error.get('message', 'Unknown error')}")
            sys.exit(1)
        
    except ConnectionRefusedError:
        print(f"Failed to connect to {socket_path}")
        print("Make sure Emacs MCP Server is running with: M-x mcp-server-start-unix")
        sys.exit(1)
    except Exception as e:
        print(f"Unexpected error: {e}")
        sys.exit(1)
    finally:
        try:
            sock.close()
            print("Disconnected from server")
        except:
            pass


if __name__ == "__main__":
    main()