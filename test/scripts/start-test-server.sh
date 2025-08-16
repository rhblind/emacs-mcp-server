#!/bin/bash

# start-test-server.sh - Start Emacs with MCP server for testing

set -euo pipefail

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $*"
}

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CONFIG_FILE="$SCRIPT_DIR/../config/minimal-test-config.el"
PIDFILE="$SCRIPT_DIR/test-emacs.pid"
LOGFILE="$SCRIPT_DIR/test-emacs.log"

# Function to check if server is running
is_server_running() {
    if [[ -f "$PIDFILE" ]]; then
        local pid=$(cat "$PIDFILE")
        if kill -0 "$pid" 2>/dev/null; then
            return 0
        else
            rm -f "$PIDFILE"
            return 1
        fi
    fi
    return 1
}

# Function to start server
start_server() {
    if is_server_running; then
        log_warning "Test server is already running (PID: $(cat "$PIDFILE"))"
        return 0
    fi
    
    log_info "Starting Emacs test server..."
    log_info "Config file: $CONFIG_FILE"
    log_info "Log file: $LOGFILE"
    
    # Start Emacs in background with minimal config
    emacs -Q --batch \
          --load "$CONFIG_FILE" \
          > "$LOGFILE" 2>&1 &
    
    local emacs_pid=$!
    echo "$emacs_pid" > "$PIDFILE"
    
    log_info "Emacs started with PID: $emacs_pid"
    
    # Wait a bit for server to initialize
    log_info "Waiting for server to initialize..."
    sleep 3
    
    # Check if process is still running
    if kill -0 "$emacs_pid" 2>/dev/null; then
        log_success "Test server started successfully"
        log_info "Check log file for details: $LOGFILE"
        return 0
    else
        log_error "Emacs process died during startup"
        log_error "Check log file for errors: $LOGFILE"
        rm -f "$PIDFILE"
        return 1
    fi
}

# Function to stop server
stop_server() {
    if ! is_server_running; then
        log_warning "Test server is not running"
        return 0
    fi
    
    local pid=$(cat "$PIDFILE")
    log_info "Stopping test server (PID: $pid)..."
    
    if kill "$pid" 2>/dev/null; then
        # Wait for process to exit
        for i in {1..10}; do
            if ! kill -0 "$pid" 2>/dev/null; then
                break
            fi
            sleep 0.5
        done
        
        # Force kill if still running
        if kill -0 "$pid" 2>/dev/null; then
            log_warning "Process didn't exit gracefully, force killing..."
            kill -9 "$pid" 2>/dev/null || true
        fi
        
        rm -f "$PIDFILE"
        log_success "Test server stopped"
    else
        log_error "Failed to stop process $pid"
        rm -f "$PIDFILE"
        return 1
    fi
}

# Function to show status
show_status() {
    if is_server_running; then
        local pid=$(cat "$PIDFILE")
        log_success "Test server is running (PID: $pid)"
        
        # Try to find socket
        local runtime_dir="${XDG_RUNTIME_DIR:-/tmp}"
        local socket_dir="$runtime_dir/emacs-mcp-server"
        if [[ -d "$socket_dir" ]]; then
            log_info "Socket directory: $socket_dir"
            for socket in "$socket_dir"/*.sock; do
                if [[ -S "$socket" ]]; then
                    log_info "Found socket: $socket"
                fi
            done
        fi
    else
        log_warning "Test server is not running"
    fi
}

# Function to show logs
show_logs() {
    if [[ -f "$LOGFILE" ]]; then
        log_info "Recent log entries:"
        tail -20 "$LOGFILE"
    else
        log_warning "No log file found at: $LOGFILE"
    fi
}

# Main function
main() {
    case "${1:-start}" in
        start)
            start_server
            ;;
        stop)
            stop_server
            ;;
        restart)
            stop_server
            sleep 1
            start_server
            ;;
        status)
            show_status
            ;;
        logs)
            show_logs
            ;;
        *)
            echo "Usage: $0 {start|stop|restart|status|logs}"
            echo
            echo "Commands:"
            echo "  start   - Start the test server"
            echo "  stop    - Stop the test server"
            echo "  restart - Restart the test server"
            echo "  status  - Show server status and socket info"
            echo "  logs    - Show recent log entries"
            exit 1
            ;;
    esac
}

# Check if config file exists
if [[ ! -f "$CONFIG_FILE" ]]; then
    log_error "Config file not found: $CONFIG_FILE"
    exit 1
fi

# Run main function
main "$@"