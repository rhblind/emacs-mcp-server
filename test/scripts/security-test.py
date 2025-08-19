#!/usr/bin/env python3
"""
Security test script for MCP Server
Tests that sensitive file access is properly blocked
"""

import sys
import socket
import json
import time

def test_sensitive_file_access(socket_path):
    """Test that sensitive files are properly protected"""
    try:
        # Connect to the MCP server
        sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        sock.settimeout(5)
        sock.connect(socket_path)
        
        # Send initialization
        init_msg = {
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {
                "protocolVersion": "draft",
                "capabilities": {},
                "clientInfo": {"name": "security-test", "version": "1.0.0"}
            }
        }
        
        send_message(sock, init_msg)
        response = receive_message(sock)
        print(f"Init response: {response}")
        
        # Test sensitive file access - should be denied
        sensitive_files = [
            "~/.authinfo",
            "~/.netrc", 
            "~/.ssh/id_rsa",
            "/etc/passwd"
        ]
        
        for file_path in sensitive_files:
            print(f"\nTesting access to sensitive file: {file_path}")
            
            # Try to read the file using eval-elisp
            test_msg = {
                "jsonrpc": "2.0",
                "id": 2,
                "method": "tools/call",
                "params": {
                    "name": "eval-elisp",
                    "arguments": {
                        "expression": f"(if (file-exists-p \"{file_path}\") (with-temp-buffer (insert-file-contents \"{file_path}\") (buffer-string)) \"File not found\")"
                    }
                }
            }
            
            send_message(sock, test_msg)
            response = receive_message(sock)
            print(f"Response: {response}")
            
            # Check if access was denied (should contain error about permission)
            if response and "error" in response:
                if "Permission denied" in str(response["error"]) or "sensitive file" in str(response["error"]):
                    print(f"✓ PASS: {file_path} access properly denied")
                else:
                    print(f"✗ FAIL: {file_path} access not properly protected")
            else:
                print(f"✗ FAIL: {file_path} access should have been denied")
        
        # Test dangerous function access
        print(f"\nTesting dangerous function access...")
        dangerous_msg = {
            "jsonrpc": "2.0", 
            "id": 3,
            "method": "tools/call",
            "params": {
                "name": "eval-elisp",
                "arguments": {
                    "expression": "(getenv \"HOME\")"
                }
            }
        }
        
        send_message(sock, dangerous_msg)
        response = receive_message(sock)
        print(f"Dangerous function response: {response}")
        
        if response and "error" in response:
            if "Permission denied" in str(response["error"]):
                print("✓ PASS: Dangerous function access properly denied")
            else:
                print("✗ FAIL: Dangerous function should be denied")
        else:
            print("✗ FAIL: Dangerous function access should have been denied")
        
        sock.close()
        print("\n✓ Security tests completed")
        
    except Exception as e:
        print(f"Error running security tests: {e}")
        return False
    
    return True

def send_message(sock, message):
    """Send a JSON-RPC message"""
    msg_str = json.dumps(message) + "\n"
    sock.send(msg_str.encode('utf-8'))

def receive_message(sock):
    """Receive a JSON-RPC message"""
    try:
        data = sock.recv(4096).decode('utf-8')
        if data:
            return json.loads(data.strip())
    except json.JSONDecodeError as e:
        print(f"JSON decode error: {e}")
        print(f"Raw data: {data}")
    except Exception as e:
        print(f"Receive error: {e}")
    return None

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python3 security-test.py <socket_path>")
        sys.exit(1)
    
    socket_path = sys.argv[1]
    print(f"Testing security with socket: {socket_path}")
    
    if test_sensitive_file_access(socket_path):
        print("Security tests completed successfully")
        sys.exit(0)
    else:
        print("Security tests failed")
        sys.exit(1)