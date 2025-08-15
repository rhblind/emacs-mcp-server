#!/usr/bin/env python3
"""
Unix Socket Client for Emacs MCP Server

This script demonstrates how to connect to the Emacs MCP Server via Unix domain sockets
and execute MCP tool calls.

Usage:
    python unix-socket-client.py [socket_path]

If socket_path is not provided, it will attempt to discover the socket automatically.
"""

import socket
import json
import sys
import os
import time
import argparse
import glob
import stat
from typing import Optional, Dict, Any, List


class EmacsMCPClient:
    """Client for communicating with Emacs MCP Server via Unix domain sockets."""
    
    def __init__(self, socket_path: Optional[str] = None):
        self.socket_path = socket_path or self._discover_socket()
        self.sock: Optional[socket.socket] = None
        self.request_id = 0
        self.initialized = False
        
    def _discover_socket(self) -> str:
        """Attempt to discover the Unix socket path automatically.
        
        Tries predictable socket names first, then falls back to PID-based discovery.
        """
        base_dir = os.getenv('XDG_RUNTIME_DIR', '/tmp')
        username = os.getenv('USER', 'unknown')
        
        # Predictable socket names (try in order of preference)
        predictable_names = [
            f"{base_dir}/mcp-server-primary.sock",      # Fixed "primary" name
            f"{base_dir}/mcp-server-{username}.sock",   # User-based naming  
            f"/tmp/mcp-server-primary.sock",            # Alternative location
            f"/tmp/mcp-server-{username}.sock"          # Alternative user-based
        ]
        
        # Try predictable names first
        for socket_path in predictable_names:
            if os.path.exists(socket_path) and self._is_socket(socket_path):
                print(f"Found predictable socket: {socket_path}")
                return socket_path
        
        # Fall back to PID-based discovery patterns
        search_patterns = [
            f"{base_dir}/mcp-server-*.sock",
            "/tmp/mcp-server-*.sock"
        ]
        
        for pattern in search_patterns:
            sockets = glob.glob(pattern)
            if sockets:
                # Filter to only actual socket files
                valid_sockets = [s for s in sockets if self._is_socket(s)]
                if valid_sockets:
                    # Return the most recently modified socket
                    latest_socket = max(valid_sockets, key=os.path.getmtime)
                    print(f"Found PID-based socket: {latest_socket}")
                    return latest_socket
        
        print("No existing sockets found, using default")
        # Default fallback
        return "/tmp/mcp-server-primary.sock"
    
    def _is_socket(self, path: str) -> bool:
        """Check if path is a valid socket file."""
        try:
            return os.path.exists(path) and stat.S_ISSOCK(os.stat(path).st_mode)
        except (OSError, AttributeError):
            return False
    
    def connect(self) -> bool:
        """Connect to the Unix domain socket."""
        try:
            self.sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            self.sock.connect(self.socket_path)
            print(f"Connected to Emacs MCP Server at {self.socket_path}")
            return True
        except (ConnectionRefusedError, FileNotFoundError) as e:
            print(f"Failed to connect to {self.socket_path}: {e}")
            print("Make sure the Emacs MCP Server is running with: M-x mcp-server-start-unix")
            return False
        except Exception as e:
            print(f"Unexpected error connecting: {e}")
            return False
    
    def disconnect(self):
        """Close the socket connection."""
        if self.sock:
            self.sock.close()
            self.sock = None
            print("Disconnected from server")
    
    def _send_message(self, message: Dict[str, Any]) -> bool:
        """Send a JSON-RPC message to the server."""
        if not self.sock:
            print("Not connected to server")
            return False
        
        try:
            json_msg = json.dumps(message) + "\n"
            self.sock.send(json_msg.encode('utf-8'))
            return True
        except Exception as e:
            print(f"Error sending message: {e}")
            return False
    
    def _receive_message(self) -> Optional[Dict[str, Any]]:
        """Receive a JSON-RPC message from the server."""
        if not self.sock:
            return None
        
        try:
            # Read until we get a complete line
            buffer = ""
            while "\n" not in buffer:
                chunk = self.sock.recv(4096).decode('utf-8')
                if not chunk:
                    return None
                buffer += chunk
            
            # Extract the first complete message
            line, remaining = buffer.split("\n", 1)
            if line.strip():
                return json.loads(line.strip())
        except Exception as e:
            print(f"Error receiving message: {e}")
        
        return None
    
    def _next_request_id(self) -> int:
        """Generate the next request ID."""
        self.request_id += 1
        return self.request_id
    
    def initialize(self) -> bool:
        """Initialize the MCP session."""
        init_message = {
            "jsonrpc": "2.0",
            "id": self._next_request_id(),
            "method": "initialize",
            "params": {
                "protocolVersion": "draft",
                "capabilities": {},
                "clientInfo": {
                    "name": "python-unix-client",
                    "version": "1.0.0"
                }
            }
        }
        
        if not self._send_message(init_message):
            return False
        
        response = self._receive_message()
        if response and response.get("id") == init_message["id"]:
            if "result" in response:
                print("Initialization successful")
                print(f"Server: {response['result'].get('serverInfo', {}).get('name', 'Unknown')}")
                
                # Send initialized notification
                self._send_message({
                    "jsonrpc": "2.0",
                    "method": "notifications/initialized"
                })
                
                self.initialized = True
                return True
            else:
                print(f"Initialization failed: {response.get('error', {}).get('message', 'Unknown error')}")
        else:
            print("Invalid initialization response")
        
        return False
    
    def list_tools(self) -> Optional[List[Dict[str, Any]]]:
        """List available tools."""
        if not self.initialized:
            print("Client not initialized")
            return None
        
        request = {
            "jsonrpc": "2.0",
            "id": self._next_request_id(),
            "method": "tools/list"
        }
        
        if not self._send_message(request):
            return None
        
        response = self._receive_message()
        if response and response.get("id") == request["id"]:
            if "result" in response:
                return response["result"].get("tools", [])
            else:
                print(f"Error listing tools: {response.get('error', {}).get('message', 'Unknown error')}")
        
        return None
    
    def call_tool(self, tool_name: str, arguments: Dict[str, Any] = None) -> Optional[Dict[str, Any]]:
        """Call a specific tool."""
        if not self.initialized:
            print("Client not initialized")
            return None
        
        request = {
            "jsonrpc": "2.0",
            "id": self._next_request_id(),
            "method": "tools/call",
            "params": {
                "name": tool_name,
                "arguments": arguments or {}
            }
        }
        
        if not self._send_message(request):
            return None
        
        response = self._receive_message()
        if response and response.get("id") == request["id"]:
            if "result" in response:
                return response["result"]
            else:
                error = response.get("error", {})
                print(f"Tool call failed: {error.get('message', 'Unknown error')}")
                return {"error": error}
        
        return None


def interactive_mode(client: EmacsMCPClient):
    """Run the client in interactive mode."""
    print("\nEmacs MCP Client - Interactive Mode")
    print("Commands:")
    print("  list - List available tools")
    print("  call <tool> [args] - Call a tool (args as JSON)")
    print("  eval <expression> - Evaluate elisp expression")
    print("  status - Get server status")
    print("  quit - Exit")
    print()
    
    while True:
        try:
            command = input("> ").strip()
            if not command:
                continue
            
            parts = command.split(None, 1)
            cmd = parts[0].lower()
            
            if cmd == "quit" or cmd == "exit":
                break
            elif cmd == "list":
                tools = client.list_tools()
                if tools:
                    print(f"Available tools ({len(tools)}):")
                    for tool in tools:
                        print(f"  - {tool.get('name', 'Unknown')}: {tool.get('description', 'No description')}")
                else:
                    print("No tools available or error occurred")
            
            elif cmd == "call" and len(parts) > 1:
                call_parts = parts[1].split(None, 1)
                tool_name = call_parts[0]
                
                args = {}
                if len(call_parts) > 1:
                    try:
                        args = json.loads(call_parts[1])
                    except json.JSONDecodeError:
                        print("Invalid JSON arguments")
                        continue
                
                result = client.call_tool(tool_name, args)
                if result:
                    if "error" in result:
                        print(f"Error: {result['error']}")
                    else:
                        content = result.get("content", [])
                        for item in content:
                            if item.get("type") == "text":
                                print(item.get("text", ""))
            
            elif cmd == "eval" and len(parts) > 1:
                expression = parts[1]
                result = client.call_tool("eval-elisp", {"expression": expression})
                if result:
                    content = result.get("content", [])
                    for item in content:
                        if item.get("type") == "text":
                            print(item.get("text", ""))
            
            elif cmd == "status":
                # Try to get some basic info
                result = client.call_tool("get-variable", {"variable": "emacs-version"})
                if result:
                    content = result.get("content", [])
                    for item in content:
                        if item.get("type") == "text":
                            print(f"Server running, Emacs version info: {item.get('text', '')}")
            
            else:
                print("Unknown command. Type 'quit' to exit.")
        
        except KeyboardInterrupt:
            print("\nInterrupted")
            break
        except EOFError:
            print("\nEOF")
            break


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(description="Connect to Emacs MCP Server via Unix socket")
    parser.add_argument("socket_path", nargs="?", help="Path to Unix domain socket")
    parser.add_argument("--interactive", "-i", action="store_true", help="Run in interactive mode")
    parser.add_argument("--list-tools", action="store_true", help="List available tools and exit")
    parser.add_argument("--eval", help="Evaluate elisp expression and exit")
    
    args = parser.parse_args()
    
    client = EmacsMCPClient(args.socket_path)
    
    if not client.connect():
        sys.exit(1)
    
    try:
        if not client.initialize():
            print("Failed to initialize client")
            sys.exit(1)
        
        if args.list_tools:
            tools = client.list_tools()
            if tools:
                print("Available tools:")
                for tool in tools:
                    print(f"  {tool.get('name', 'Unknown')}: {tool.get('description', 'No description')}")
            else:
                print("No tools available")
        
        elif args.eval:
            result = client.call_tool("eval-elisp", {"expression": args.eval})
            if result:
                content = result.get("content", [])
                for item in content:
                    if item.get("type") == "text":
                        print(item.get("text", ""))
        
        elif args.interactive:
            interactive_mode(client)
        
        else:
            # Default: show available tools and run interactive mode
            print("Connected to Emacs MCP Server")
            tools = client.list_tools()
            if tools:
                print(f"Found {len(tools)} available tools")
            interactive_mode(client)
    
    finally:
        client.disconnect()


if __name__ == "__main__":
    main()