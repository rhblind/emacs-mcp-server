#!/usr/bin/env python3
"""
Simple security test - verify basic functionality
"""

import sys
import socket
import json

def test_basic_functionality(socket_path):
    """Test basic eval-elisp functionality works"""
    try:
        sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        sock.settimeout(5)
        sock.connect(socket_path)
        
        # Initialize
        init_msg = {
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {
                "protocolVersion": "draft",
                "capabilities": {},
                "clientInfo": {"name": "simple-test", "version": "1.0.0"}
            }
        }
        
        sock.send((json.dumps(init_msg) + "\n").encode())
        response = sock.recv(4096).decode()
        print(f"Init successful: {bool(response)}")
        
        # Test simple safe operation
        test_msg = {
            "jsonrpc": "2.0",
            "id": 2,
            "method": "tools/call",
            "params": {
                "name": "eval-elisp",
                "arguments": {
                    "expression": "(+ 1 2 3)"
                }
            }
        }
        
        sock.send((json.dumps(test_msg) + "\n").encode())
        response = sock.recv(4096).decode()
        print(f"Math test response: {response}")
        
        if "6" in response:
            print("✓ PASS: Basic functionality works")
        else:
            print("✗ FAIL: Basic functionality failed")
        
        sock.close()
        return True
        
    except Exception as e:
        print(f"Error: {e}")
        return False

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python3 simple-security-test.py <socket_path>")
        sys.exit(1)
    
    socket_path = sys.argv[1]
    print(f"Testing basic functionality with socket: {socket_path}")
    
    if test_basic_functionality(socket_path):
        print("Basic functionality test passed")
    else:
        print("Basic functionality test failed")