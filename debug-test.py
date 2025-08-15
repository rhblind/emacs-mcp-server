#!/usr/bin/env python3
"""
Debug test for MCP server - step by step debugging
"""

import socket
import json
import sys

def send_and_receive(sock, message, description):
    print(f"\n=== {description} ===")
    print("Sending:", json.dumps(message, indent=2))
    
    try:
        json_msg = json.dumps(message) + "\n"
        sock.send(json_msg.encode('utf-8'))
        
        # Read response
        buffer = ""
        while "\n" not in buffer:
            chunk = sock.recv(4096).decode('utf-8')
            if not chunk:
                print("No response received")
                return None
            buffer += chunk
        
        line, remaining = buffer.split("\n", 1)
        if line.strip():
            response = json.loads(line.strip())
            print("Received:", json.dumps(response, indent=2))
            return response
    except Exception as e:
        print(f"Error: {e}")
        return None

def main():
    socket_path = "/Users/aa646/Library/Caches/emacs-mcp-server/emacs-mcp-server-test-server.sock"
    
    print(f"Connecting to {socket_path}")
    
    try:
        sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        sock.connect(socket_path)
        print("Connected!")
        
        # Step 1: Initialize
        init_msg = {
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {
                "protocolVersion": "draft",
                "capabilities": {},
                "clientInfo": {
                    "name": "debug-test",
                    "version": "1.0.0"
                }
            }
        }
        
        response = send_and_receive(sock, init_msg, "INITIALIZATION")
        if not response or "result" not in response:
            print("Initialization failed!")
            return
        
        # Step 2: Initialized notification
        init_notification = {
            "jsonrpc": "2.0",
            "method": "notifications/initialized"
        }
        
        print(f"\n=== INITIALIZED NOTIFICATION ===")
        print("Sending:", json.dumps(init_notification, indent=2))
        json_msg = json.dumps(init_notification) + "\n"
        sock.send(json_msg.encode('utf-8'))
        print("Notification sent (no response expected)")
        
        # Step 3: List tools
        list_tools_msg = {
            "jsonrpc": "2.0",
            "id": 2,
            "method": "tools/list"
        }
        
        response = send_and_receive(sock, list_tools_msg, "LIST TOOLS")
        if response and "result" in response:
            tools = response["result"].get("tools", [])
            print(f"Available tools: {[t.get('name') for t in tools]}")
        
        # Step 4: Simple eval test first
        simple_eval_msg = {
            "jsonrpc": "2.0",
            "id": 3,
            "method": "tools/call",
            "params": {
                "name": "eval-elisp",
                "arguments": {
                    "expression": "(+ 1 2 3)"
                }
            }
        }
        
        response = send_and_receive(sock, simple_eval_msg, "SIMPLE EVAL TEST")
        
        # Step 5: Message test
        message_eval_msg = {
            "jsonrpc": "2.0",
            "id": 4,
            "method": "tools/call",
            "params": {
                "name": "eval-elisp",
                "arguments": {
                    "expression": "(message \"Hello from debug test!\")"
                }
            }
        }
        
        response = send_and_receive(sock, message_eval_msg, "MESSAGE TEST")
        
    except Exception as e:
        print(f"Error: {e}")
    finally:
        sock.close()

if __name__ == "__main__":
    main()