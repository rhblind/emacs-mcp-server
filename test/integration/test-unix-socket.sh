#!/bin/bash

# test-unix-socket.sh - Shell script to test Emacs MCP Server via Unix domain socket
#
# This script uses socat to communicate with the Emacs MCP Server over Unix domain sockets.
# It demonstrates the JSON-RPC protocol flow and can be used for basic testing.
#
# Requirements:
#   - socat (for Unix socket communication)
#   - jq (for JSON formatting, optional but recommended)
#
# Usage:
#   ./test-unix-socket.sh [socket_path]
#   
#   If socket_path is not provided, the script will attempt to discover it automatically.

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
SOCKET_PATH=""
VERBOSE=false
TIMEOUT=10

# Utility functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*"
}

# Check dependencies
check_dependencies() {
    local missing_deps=()
    
    if ! command -v socat &> /dev/null; then
        missing_deps+=("socat")
    fi
    
    if [[ ${#missing_deps[@]} -gt 0 ]]; then
        log_error "Missing required dependencies: ${missing_deps[*]}"
        log_info "Install with: brew install socat (macOS) or apt-get install socat (Ubuntu)"
        exit 1
    fi
    
    if ! command -v jq &> /dev/null; then
        log_warning "jq not found - JSON output will not be formatted"
    fi
}

# Discover socket path
discover_socket() {
    # Enable nullglob to handle patterns that don't match
    shopt -s nullglob
    
    local patterns=(
        "/tmp/mcp-server-*.sock"
        "${XDG_RUNTIME_DIR:-/tmp}/mcp-server-*.sock"
    )
    
    log_info "Searching for socket files..."
    
    for pattern in "${patterns[@]}"; do
        log_info "Checking pattern: $pattern"
        local sockets=($pattern)
        for socket in "${sockets[@]}"; do
            log_info "Found potential socket: $socket"
            if [[ -S "$socket" ]]; then
                log_success "Valid socket found: $socket"
                echo "$socket"
                shopt -u nullglob
                return 0
            else
                log_warning "File exists but is not a socket: $socket"
            fi
        done
    done
    
    # Try a broader search
    log_info "Trying broader search in /tmp/..."
    local all_sockets=(/tmp/*.sock)
    for socket in "${all_sockets[@]}"; do
        if [[ -S "$socket" && "$socket" == *"emacs"* ]]; then
            log_success "Found Emacs socket: $socket"
            echo "$socket"
            shopt -u nullglob
            return 0
        fi
    done
    
    # Disable nullglob
    shopt -u nullglob
    
    log_warning "No socket found, using default"
    # Default fallback
    echo "/tmp/mcp-server.sock"
}

# Format JSON output
format_json() {
    if command -v jq &> /dev/null; then
        jq -C '.'
    else
        cat
    fi
}

# Send JSON-RPC message
send_message() {
    local message="$1"
    local description="$2"
    
    log_info "Sending: $description"
    if [[ "$VERBOSE" == "true" ]]; then
        echo "$message" | format_json
    fi
    
    local response
    response=$(echo "$message" | timeout "$TIMEOUT" socat - "UNIX-CONNECT:$SOCKET_PATH" 2>/dev/null || true)
    
    if [[ -n "$response" ]]; then
        log_success "Response received:"
        echo "$response" | format_json
        echo
        return 0
    else
        log_error "No response received or connection failed"
        return 1
    fi
}

# Test initialization
test_initialize() {
    local init_message='{
        "jsonrpc": "2.0",
        "id": 1,
        "method": "initialize",
        "params": {
            "protocolVersion": "draft",
            "capabilities": {},
            "clientInfo": {
                "name": "shell-test-client",
                "version": "1.0.0"
            }
        }
    }'
    
    send_message "$init_message" "Initialization request"
}

# Test initialized notification
test_initialized() {
    local initialized_message='{
        "jsonrpc": "2.0",
        "method": "notifications/initialized"
    }'
    
    # Note: This is a notification, so no response expected
    log_info "Sending: Initialized notification"
    echo "$initialized_message" | socat - "UNIX-CONNECT:$SOCKET_PATH" 2>/dev/null || true
    log_success "Initialized notification sent"
    echo
}

# Test tools list
test_tools_list() {
    local tools_message='{
        "jsonrpc": "2.0",
        "id": 2,
        "method": "tools/list"
    }'
    
    send_message "$tools_message" "Tools list request"
}

# Test eval-elisp tool
test_eval_elisp() {
    local eval_message='{
        "jsonrpc": "2.0",
        "id": 3,
        "method": "tools/call",
        "params": {
            "name": "eval-elisp",
            "arguments": {
                "expression": "(+ 2 3 5)"
            }
        }
    }'
    
    send_message "$eval_message" "Elisp evaluation: (+ 2 3 5)"
}

# Test get-buffer-list tool
test_buffer_list() {
    local buffer_message='{
        "jsonrpc": "2.0",
        "id": 4,
        "method": "tools/call",
        "params": {
            "name": "get-buffer-list",
            "arguments": {}
        }
    }'
    
    send_message "$buffer_message" "Get buffer list"
}

# Test get-variable tool
test_get_variable() {
    local variable_message='{
        "jsonrpc": "2.0",
        "id": 5,
        "method": "tools/call",
        "params": {
            "name": "get-variable",
            "arguments": {
                "variable": "emacs-version"
            }
        }
    }'
    
    send_message "$variable_message" "Get emacs-version variable"
}

# Interactive mode
interactive_mode() {
    log_info "Entering interactive mode. Type JSON-RPC messages or 'quit' to exit."
    log_info "Example: {\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\"}"
    echo
    
    while true; do
        echo -n "JSON-RPC> "
        read -r input
        
        if [[ "$input" == "quit" || "$input" == "exit" ]]; then
            break
        fi
        
        if [[ -n "$input" ]]; then
            local response
            response=$(echo "$input" | timeout "$TIMEOUT" socat - "UNIX-CONNECT:$SOCKET_PATH" 2>/dev/null || true)
            
            if [[ -n "$response" ]]; then
                echo "$response" | format_json
            else
                log_error "No response or connection failed"
            fi
            echo
        fi
    done
}

# Main test sequence
run_full_test() {
    log_info "Running full MCP server test sequence"
    echo
    
    # Check if socket exists
    if [[ ! -S "$SOCKET_PATH" ]]; then
        log_error "Socket not found at: $SOCKET_PATH"
        log_info "Make sure Emacs MCP Server is running with: M-x mcp-server-start-unix"
        exit 1
    fi
    
    log_success "Found socket at: $SOCKET_PATH"
    echo
    
    # Run test sequence
    test_initialize || return 1
    sleep 0.5
    
    test_initialized || return 1
    sleep 0.5
    
    test_tools_list || return 1
    sleep 0.5
    
    test_eval_elisp || return 1
    sleep 0.5
    
    test_buffer_list || return 1
    sleep 0.5
    
    test_get_variable || return 1
    
    log_success "All tests completed successfully!"
}

# Parse command line arguments
usage() {
    cat << EOF
Usage: $0 [OPTIONS] [SOCKET_PATH]

Test Emacs MCP Server via Unix domain socket using socat.

OPTIONS:
    -h, --help          Show this help message
    -v, --verbose       Enable verbose output
    -i, --interactive   Run in interactive mode
    -t, --timeout N     Set timeout in seconds (default: 10)
    -s, --socket PATH   Specify socket path explicitly
    --init-only         Run only initialization test
    --tools-only        Run only tools test
    --eval EXPR         Evaluate specific elisp expression

EXAMPLES:
    $0                                  # Auto-discover socket and run full test
    $0 /tmp/custom.sock                # Use specific socket path
    $0 -s /tmp/mcp-server-12345.sock  # Specify socket explicitly
    $0 -i                              # Interactive mode
    $0 --eval "(buffer-name)"          # Evaluate expression
    $0 -v --tools-only                 # Verbose tools test only

To find your socket path, run in Emacs:
    M-x mcp-server-get-socket-path

EOF
}

# Parse options
TEMP=$(getopt -o hvits: --long help,verbose,interactive,timeout:,socket:,init-only,tools-only,eval: -n "$0" -- "$@")
eval set -- "$TEMP"

INTERACTIVE=false
INIT_ONLY=false
TOOLS_ONLY=false
EVAL_EXPR=""
EXPLICIT_SOCKET=""

while true; do
    case "$1" in
        -h|--help)
            usage
            exit 0
            ;;
        -v|--verbose)
            VERBOSE=true
            shift
            ;;
        -i|--interactive)
            INTERACTIVE=true
            shift
            ;;
        -t|--timeout)
            TIMEOUT="$2"
            shift 2
            ;;
        -s|--socket)
            EXPLICIT_SOCKET="$2"
            shift 2
            ;;
        --init-only)
            INIT_ONLY=true
            shift
            ;;
        --tools-only)
            TOOLS_ONLY=true
            shift
            ;;
        --eval)
            EVAL_EXPR="$2"
            shift 2
            ;;
        --)
            shift
            break
            ;;
        *)
            log_error "Internal error parsing arguments"
            exit 1
            ;;
    esac
done

# Collect remaining positional arguments
REMAINING_ARGS=("$@")
echo "DEBUG: Remaining args after getopt: ${REMAINING_ARGS[*]}"
echo "DEBUG: Number of remaining args: ${#REMAINING_ARGS[@]}"

# Get socket path
if [[ -n "$EXPLICIT_SOCKET" ]]; then
    SOCKET_PATH="$EXPLICIT_SOCKET"
    echo "DEBUG: Using explicit socket: $EXPLICIT_SOCKET"
elif [[ ${#REMAINING_ARGS[@]} -gt 0 ]]; then
    SOCKET_PATH="${REMAINING_ARGS[0]}"
    echo "DEBUG: Using argument socket: ${REMAINING_ARGS[0]}"
else
    SOCKET_PATH=$(discover_socket)
    echo "DEBUG: Using discovered socket: $SOCKET_PATH"
fi

echo "DEBUG: Final SOCKET_PATH='$SOCKET_PATH'"

# Main execution
main() {
    check_dependencies
    
    log_info "Emacs MCP Server Test Script"
    log_info "Socket path: $SOCKET_PATH"
    log_info "Timeout: ${TIMEOUT}s"
    echo
    
    if [[ -n "$EVAL_EXPR" ]]; then
        # Evaluate specific expression
        log_info "Testing specific elisp expression: $EVAL_EXPR"
        
        test_initialize || exit 1
        test_initialized || exit 1
        
        local eval_message
        eval_message=$(cat << EOF
{
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/call",
    "params": {
        "name": "eval-elisp",
        "arguments": {
            "expression": "$EVAL_EXPR"
        }
    }
}
EOF
)
        send_message "$eval_message" "Evaluating: $EVAL_EXPR"
        
    elif [[ "$INTERACTIVE" == "true" ]]; then
        interactive_mode
        
    elif [[ "$INIT_ONLY" == "true" ]]; then
        test_initialize
        test_initialized
        
    elif [[ "$TOOLS_ONLY" == "true" ]]; then
        test_initialize || exit 1
        test_initialized || exit 1
        test_tools_list || exit 1
        
    else
        run_full_test
    fi
}

# Run main function
main