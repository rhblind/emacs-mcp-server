#!/usr/bin/env python3
"""
Emacs MCP Server Wrapper

This script acts as a bridge between MCP clients and the Emacs MCP Server
running via Unix domain sockets. It can be used with tools like Claude Desktop
or uvx for easy MCP integration.

Usage:
    python emacs-mcp-wrapper.py [--socket-name NAME] [--timeout SECONDS]
    
    # For uvx/pipx installation:
    uvx mcp-server --scope user

Environment Variables:
    EMACS_MCP_SOCKET_NAME: Override socket name (default: "primary")
    EMACS_MCP_SOCKET_PATH: Full path to socket (overrides discovery)
    EMACS_MCP_TIMEOUT: Connection timeout in seconds (default: 10)
"""

import sys
import os
import socket
import json
import time
import argparse
import glob
import stat
import signal
import threading
from typing import Optional


class EmacsMCPWrapper:
    """Wrapper that connects MCP clients to Emacs MCP Server via Unix sockets."""
    
    def __init__(self, socket_name: str = "primary", timeout: int = 10):
        self.socket_name = socket_name
        self.timeout = timeout
        self.socket_path = self._find_socket()
        self.client_socket: Optional[socket.socket] = None
        self.running = True
        
    def _find_socket(self) -> str:
        """Find the Emacs MCP Server socket."""
        # Check if explicit path is provided
        explicit_path = os.getenv('EMACS_MCP_SOCKET_PATH')
        if explicit_path and os.path.exists(explicit_path):
            return explicit_path
        
        # Use socket name from environment or parameter
        socket_name = os.getenv('EMACS_MCP_SOCKET_NAME', self.socket_name)
        
        # Build socket paths to try
        # Check macOS-specific cache directory first
        if sys.platform == 'darwin':
            cache_dir = os.path.expanduser('~/Library/Caches/emacs-mcp-server')
            base_dirs = [cache_dir, os.getenv('XDG_RUNTIME_DIR', '/tmp'), '/tmp']
        else:
            xdg_dir = os.path.expanduser('~/.emacs.d/emacs-mcp-server')
            base_dirs = [os.getenv('XDG_RUNTIME_DIR', xdg_dir), xdg_dir, '/tmp']
        
        username = os.getenv('USER', 'unknown')
        
        socket_candidates = []
        
        # Add configured socket name paths
        for base_dir in base_dirs:
            if socket_name == "user":
                socket_candidates.append(f"{base_dir}/emacs-mcp-server-{username}.sock")
            elif socket_name == "session":
                # Try to find session-based sockets
                session_pattern = f"{base_dir}/emacs-mcp-server-{username}-*.sock"
                session_sockets = glob.glob(session_pattern)
                if session_sockets:
                    # Use the most recent session socket
                    session_sockets.sort(key=os.path.getmtime, reverse=True)
                    socket_candidates.extend(session_sockets)
            else:
                # Fixed socket name
                socket_candidates.append(f"{base_dir}/emacs-mcp-server-{socket_name}.sock")
        
        # Add fallback patterns
        for base_dir in base_dirs:
            socket_candidates.extend([
                f"{base_dir}/emacs-mcp-server-primary.sock",
                f"{base_dir}/emacs-mcp-server-{username}.sock"
            ])
        
        # Find first existing socket
        for candidate in socket_candidates:
            if os.path.exists(candidate) and self._is_socket(candidate):
                return candidate
        
        # If no predictable sockets found, try PID-based discovery
        for base_dir in base_dirs:
            pattern = f"{base_dir}/emacs-mcp-server-*.sock"
            pid_sockets = glob.glob(pattern)
            valid_sockets = [s for s in pid_sockets if self._is_socket(s)]
            if valid_sockets:
                # Return the most recent
                return max(valid_sockets, key=os.path.getmtime)
        
        # Final fallback
        return f"/tmp/emacs-mcp-server-{socket_name}.sock"
    
    def _is_socket(self, path: str) -> bool:
        """Check if path is a valid socket file."""
        try:
            return os.path.exists(path) and stat.S_ISSOCK(os.stat(path).st_mode)
        except (OSError, AttributeError):
            return False
    
    def connect(self) -> bool:
        """Connect to the Emacs MCP Server."""
        try:
            self.client_socket = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            # Set timeout only for initial connection, then remove it
            self.client_socket.settimeout(self.timeout)
            self.client_socket.connect(self.socket_path)
            # Remove timeout for persistent connection
            self.client_socket.settimeout(None)
            return True
        except (ConnectionRefusedError, FileNotFoundError, socket.timeout) as e:
            self._error(f"Failed to connect to Emacs MCP Server at {self.socket_path}: {e}")
            self._error("Make sure Emacs is running with: M-x mcp-server-start-unix")
            return False
        except Exception as e:
            self._error(f"Unexpected connection error: {e}")
            return False
    
    def run(self):
        """Run the wrapper, forwarding messages between stdin/stdout and socket."""
        if not self.connect():
            sys.exit(1)
        
        self._debug(f"Connected to Emacs MCP Server at {self.socket_path}")
        
        # Set socket to non-blocking mode for better responsiveness
        try:
            self.client_socket.setblocking(True)  # Keep blocking but remove timeout
        except Exception as e:
            self._error(f"Failed to configure socket: {e}")
            sys.exit(1)
        
        # Set up signal handlers for clean shutdown
        signal.signal(signal.SIGTERM, self._signal_handler)
        signal.signal(signal.SIGINT, self._signal_handler)
        
        # Start threads for bidirectional communication
        stdin_thread = threading.Thread(target=self._forward_stdin_to_socket, daemon=True)
        socket_thread = threading.Thread(target=self._forward_socket_to_stdout, daemon=True)
        
        stdin_thread.start()
        socket_thread.start()
        
        # Wait for threads to complete or shutdown signal
        try:
            while self.running and (stdin_thread.is_alive() or socket_thread.is_alive()):
                time.sleep(0.1)
                # Check if either thread has died unexpectedly
                if not stdin_thread.is_alive() and self.running:
                    self._debug("stdin thread died, shutting down")
                    break
                if not socket_thread.is_alive() and self.running:
                    self._debug("socket thread died, shutting down")
                    break
        except KeyboardInterrupt:
            self._debug("KeyboardInterrupt received")
        finally:
            self.shutdown()
    
    def _forward_stdin_to_socket(self):
        """Forward messages from stdin to the socket."""
        try:
            while self.running:
                try:
                    line = sys.stdin.readline()
                    if not line:  # EOF
                        self._debug("EOF received on stdin")
                        break
                    if self.client_socket:
                        self.client_socket.sendall(line.encode('utf-8'))
                except (BrokenPipeError, ConnectionResetError) as e:
                    self._error(f"Socket connection lost: {e}")
                    break
                except Exception as e:
                    self._error(f"Error forwarding stdin to socket: {e}")
                    break
        except Exception as e:
            self._error(f"Unexpected error in stdin forwarding: {e}")
        finally:
            self._debug("stdin forwarding thread exiting")
            self.running = False
    
    def _forward_socket_to_stdout(self):
        """Forward messages from socket to stdout."""
        try:
            while self.running:
                if not self.client_socket:
                    break
                
                try:
                    data = self.client_socket.recv(4096)
                    if not data:  # Connection closed by server
                        self._debug("Socket closed by server")
                        break
                    
                    sys.stdout.write(data.decode('utf-8'))
                    sys.stdout.flush()
                except (ConnectionResetError, BrokenPipeError) as e:
                    self._error(f"Socket connection lost: {e}")
                    break
                except socket.timeout:
                    # This shouldn't happen since we removed timeout, but just in case
                    continue
                except Exception as e:
                    self._error(f"Error reading from socket: {e}")
                    break
        except Exception as e:
            self._error(f"Unexpected error in socket forwarding: {e}")
        finally:
            self._debug("socket forwarding thread exiting")
            self.running = False
    
    def _signal_handler(self, signum, frame):
        """Handle shutdown signals."""
        self._debug(f"Received signal {signum}, shutting down...")
        self.shutdown()
    
    def shutdown(self):
        """Clean shutdown."""
        self._debug("Shutting down wrapper")
        self.running = False
        if self.client_socket:
            try:
                self.client_socket.shutdown(socket.SHUT_RDWR)
                self.client_socket.close()
            except Exception as e:
                self._debug(f"Error during socket shutdown: {e}")
            self.client_socket = None
    
    def _debug(self, message: str):
        """Debug logging to stderr."""
        if os.getenv('EMACS_MCP_DEBUG'):
            print(f"[EMACS-MCP-WRAPPER] {message}", file=sys.stderr)
    
    def _error(self, message: str):
        """Error logging to stderr."""
        print(f"[EMACS-MCP-WRAPPER ERROR] {message}", file=sys.stderr)


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(description="Emacs MCP Server Wrapper")
    parser.add_argument("--socket-name", default="primary",
                        help="Socket name to connect to (default: primary)")
    parser.add_argument("--timeout", type=int, default=10,
                        help="Connection timeout in seconds (default: 10)")
    parser.add_argument("--list-sockets", action="store_true",
                        help="List available socket files and exit")
    parser.add_argument("--socket-path", 
                        help="Full path to socket file (overrides discovery)")
    parser.add_argument("--test-connection", action="store_true",
                        help="Test connection to socket and exit")
    parser.add_argument("--version", action="version", version="emacs-mcp-wrapper 1.1.0")
    
    args = parser.parse_args()
    
    if args.list_sockets:
        # List available sockets
        wrapper = EmacsMCPWrapper(args.socket_name, args.timeout)
        print(f"Socket discovery for name '{args.socket_name}':")
        print(f"Found socket: {wrapper.socket_path}")
        print(f"Socket exists: {os.path.exists(wrapper.socket_path)}")
        print(f"Is valid socket: {wrapper._is_socket(wrapper.socket_path)}")
        
        # List all available sockets
        print("\nAll available emacs-mcp-server sockets:")
        search_dirs = ['/tmp', os.path.expanduser('~/Library/Caches/emacs-mcp-server'), 
                      os.path.expanduser('~/.emacs.d/emacs-mcp-server')]
        for search_dir in search_dirs:
            if os.path.exists(search_dir):
                pattern = f"{search_dir}/emacs-mcp-server-*.sock"
                sockets = glob.glob(pattern)
                for sock in sockets:
                    if wrapper._is_socket(sock):
                        print(f"  {sock}")
        return
    
    if args.test_connection:
        # Test connection and exit
        wrapper = EmacsMCPWrapper(args.socket_name, args.timeout)
        if args.socket_path:
            wrapper.socket_path = args.socket_path
        
        print(f"Testing connection to: {wrapper.socket_path}")
        if wrapper.connect():
            print("✓ Connection successful!")
            # Send a test message to verify protocol works
            test_msg = '{"jsonrpc":"2.0","id":1,"method":"tools/list"}\n'
            try:
                wrapper.client_socket.send(test_msg.encode('utf-8'))
                response = wrapper.client_socket.recv(4096).decode('utf-8')
                print(f"✓ Protocol test successful: {len(response)} bytes received")
                wrapper.shutdown()
                sys.exit(0)
            except Exception as e:
                print(f"✗ Protocol test failed: {e}")
                wrapper.shutdown()
                sys.exit(1)
        else:
            print("✗ Connection failed!")
            sys.exit(1)
    
    # Override from environment or command line
    socket_name = os.getenv('EMACS_MCP_SOCKET_NAME', args.socket_name)
    timeout = int(os.getenv('EMACS_MCP_TIMEOUT', str(args.timeout)))
    
    wrapper = EmacsMCPWrapper(socket_name, timeout)
    
    # Override socket path if provided
    if args.socket_path:
        wrapper.socket_path = args.socket_path
    
    wrapper.run()


if __name__ == "__main__":
    main()