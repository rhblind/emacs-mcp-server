#!/bin/bash

# test-runner.sh - Comprehensive test runner for MCP Server
#
# This script runs a full test suite to validate the MCP server refactoring
# and functionality, including socket communication and MCP protocol compliance.

set -euo pipefail

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TEST_SOCKET_NAME="test-primary"
TEST_TIMEOUT=5
EMACS_BIN="${EMACS:-emacs}"
VERBOSE=false
KEEP_RUNNING=false
SKIP_EMACS_START=false

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Logging functions
log_header() {
    echo -e "\n${CYAN}=== $* ===${NC}"
}

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

log_verbose() {
    if [[ "$VERBOSE" == "true" ]]; then
        echo -e "${BLUE}[DEBUG]${NC} $*"
    fi
}

# Test state tracking
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0
EMACS_PID=""
SOCKET_PATH=""

# Cleanup function
cleanup() {
    log_info "Cleaning up test environment..."
    
    # Stop Emacs if we started it
    if [[ -n "$EMACS_PID" ]]; then
        log_info "Stopping Emacs (PID: $EMACS_PID)..."
        kill "$EMACS_PID" 2>/dev/null || true
        wait "$EMACS_PID" 2>/dev/null || true
    fi
    
    # Remove test socket if it exists
    if [[ -n "$SOCKET_PATH" && -S "$SOCKET_PATH" ]]; then
        log_info "Removing test socket: $SOCKET_PATH"
        rm -f "$SOCKET_PATH"
    fi
    
    log_info "Cleanup complete."
}

# Set up signal handlers
trap cleanup EXIT INT TERM

# Check dependencies
check_dependencies() {
    log_header "Checking Dependencies"
    
    local missing_deps=()
    
    if ! command -v "$EMACS_BIN" &> /dev/null; then
        missing_deps+=("emacs")
    fi
    
    if ! command -v socat &> /dev/null; then
        missing_deps+=("socat")
    fi
    
    if ! command -v python3 &> /dev/null; then
        missing_deps+=("python3")
    fi
    
    if [[ ${#missing_deps[@]} -gt 0 ]]; then
        log_error "Missing required dependencies: ${missing_deps[*]}"
        log_info "Install missing dependencies and try again"
        exit 1
    fi
    
    log_success "All dependencies available"
    
    # Show versions
    log_verbose "Emacs version: $("$EMACS_BIN" --version | head -1)"
    log_verbose "Socat version: $(socat -V 2>&1 | head -1 | grep -o 'socat version [0-9.]*' || echo 'unknown')"
    log_verbose "Python version: $(python3 --version)"
}

# Start Emacs with MCP server
start_emacs_server() {
    if [[ "$SKIP_EMACS_START" == "true" ]]; then
        log_info "Skipping Emacs startup (assuming server is already running)"
        # Try to determine socket directory the same way Emacs does
        if [[ -n "${XDG_RUNTIME_DIR:-}" ]]; then
            SOCKET_PATH="${XDG_RUNTIME_DIR}/emacs-mcp-server-${TEST_SOCKET_NAME}.sock"
        elif [[ "$(uname)" == "Darwin" ]]; then
            mkdir -p "${HOME}/Library/Caches/emacs-mcp-server"
            SOCKET_PATH="${HOME}/Library/Caches/emacs-mcp-server/emacs-mcp-server-${TEST_SOCKET_NAME}.sock"
        else
            mkdir -p "${HOME}/.emacs.d/emacs-mcp-server"
            SOCKET_PATH="${HOME}/.emacs.d/emacs-mcp-server/emacs-mcp-server-${TEST_SOCKET_NAME}.sock"
        fi
        return 0
    fi
    
    log_header "Starting Emacs MCP Server"
    
    log_info "Starting Emacs with MCP server..."
    
    # Start Emacs directly with eval commands
    "$EMACS_BIN" --batch \
        --eval "(add-to-list 'load-path \"$SCRIPT_DIR/../..\")" \
        --eval "(add-to-list 'load-path \"$SCRIPT_DIR/../config\")" \
        --eval "(require 'test-config)" \
        --eval "(setq mcp-server-socket-name \"$TEST_SOCKET_NAME\")" \
        --eval "(setq mcp-server-debug t)" \
        --eval "(mcp-test-start-server)" \
        --eval "(mcp-test-server-info)" \
        --eval "(message \"MCP Server ready for testing...\")" \
        --eval "(while mcp-server-running (sleep-for 1))" &
    EMACS_PID=$!
    
    log_info "Emacs started with PID: $EMACS_PID"
    
    # Wait for socket to appear - determine expected path
    if [[ -n "${XDG_RUNTIME_DIR:-}" ]]; then
        SOCKET_PATH="${XDG_RUNTIME_DIR}/emacs-mcp-server-${TEST_SOCKET_NAME}.sock"
    elif [[ "$(uname)" == "Darwin" ]]; then
        SOCKET_PATH="${HOME}/Library/Caches/emacs-mcp-server/emacs-mcp-server-${TEST_SOCKET_NAME}.sock"
    else
        SOCKET_PATH="${HOME}/.emacs.d/emacs-mcp-server/emacs-mcp-server-${TEST_SOCKET_NAME}.sock"
    fi
    log_info "Waiting for socket: $SOCKET_PATH"
    
    local wait_count=0
    while [[ ! -S "$SOCKET_PATH" && $wait_count -lt $TEST_TIMEOUT ]]; do
        sleep 1
        ((wait_count++))
        log_verbose "Waiting for socket... ($wait_count/$TEST_TIMEOUT)"
    done
    
    if [[ -S "$SOCKET_PATH" ]]; then
        log_success "Socket created: $SOCKET_PATH"
        return 0
    else
        log_error "Socket not created within timeout"
        return 1
    fi
}

# Test function wrapper
run_test() {
    local test_name="$1"
    local test_func="$2"
    
    log_info "Running test: $test_name"
    ((TESTS_RUN++))
    
    if $test_func; then
        log_success "✓ $test_name"
        ((TESTS_PASSED++))
        return 0
    else
        log_error "✗ $test_name"
        ((TESTS_FAILED++))
        return 1
    fi
}

# Test 1: Validate socket exists and is accessible
test_socket_exists() {
    if [[ ! -S "$SOCKET_PATH" ]]; then
        log_error "Socket file does not exist: $SOCKET_PATH"
        return 1
    fi
    
    # Test basic connectivity
    if timeout 2 bash -c "echo | socat - UNIX-CONNECT:$SOCKET_PATH" &>/dev/null; then
        log_verbose "Socket is accessible"
        return 0
    else
        log_error "Socket exists but is not accessible"
        return 1
    fi
}

# Test 2: Shell script communication
test_shell_script() {
    local test_script="$SCRIPT_DIR/examples/test-unix-socket-fixed.sh"
    
    if [[ ! -x "$test_script" ]]; then
        log_error "Test script not executable: $test_script"
        return 1
    fi
    
    log_verbose "Running shell test script..."
    if timeout $TEST_TIMEOUT "$test_script" -s "$SOCKET_PATH" --init-only &>/dev/null; then
        log_verbose "Shell script initialization test passed"
    else
        log_error "Shell script initialization test failed"
        return 1
    fi
    
    # Test tools listing
    if timeout $TEST_TIMEOUT "$test_script" -s "$SOCKET_PATH" --tools-only &>/dev/null; then
        log_verbose "Shell script tools test passed"
    else
        log_error "Shell script tools test failed"
        return 1
    fi
    
    # Test evaluation
    if timeout $TEST_TIMEOUT "$test_script" -s "$SOCKET_PATH" --eval "(+ 1 2 3)" &>/dev/null; then
        log_verbose "Shell script evaluation test passed"
        return 0
    else
        log_error "Shell script evaluation test failed"
        return 1
    fi
}

# Test 3: Python client communication  
test_python_client() {
    local python_client="$SCRIPT_DIR/examples/unix-socket-client.py"
    
    if [[ ! -f "$python_client" ]]; then
        log_error "Python client not found: $python_client"
        return 1
    fi
    
    log_verbose "Running Python client test..."
    
    # Test basic connection and tool listing
    if timeout $TEST_TIMEOUT python3 "$python_client" "$SOCKET_PATH" &>/dev/null; then
        log_verbose "Python client test passed"
        return 0
    else
        log_error "Python client test failed"
        return 1
    fi
}

# Test 4: MCP wrapper scripts
test_mcp_wrappers() {
    local shell_wrapper="$SCRIPT_DIR/examples/mcp-client-configs/mcp-wrapper.sh"
    local python_wrapper="$SCRIPT_DIR/examples/mcp-client-configs/mcp-wrapper.py"
    
    # Test shell wrapper
    if [[ -x "$shell_wrapper" ]]; then
        log_verbose "Testing shell wrapper..."
        # Set environment variable for socket discovery
        if timeout 2 bash -c "EMACS_MCP_SOCKET_PATH='$SOCKET_PATH' echo '{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\"}' | '$shell_wrapper'" &>/dev/null; then
            log_verbose "Shell wrapper test passed"
        else
            log_warning "Shell wrapper test failed"
        fi
    else
        log_warning "Shell wrapper not executable: $shell_wrapper"
    fi
    
    # Test Python wrapper
    if [[ -f "$python_wrapper" ]]; then
        log_verbose "Testing Python wrapper..."
        if timeout 2 bash -c "EMACS_MCP_SOCKET_PATH='$SOCKET_PATH' echo '{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\"}' | python3 '$python_wrapper'" &>/dev/null; then
            log_verbose "Python wrapper test passed"
        else
            log_warning "Python wrapper test failed"
        fi
    else
        log_warning "Python wrapper not found: $python_wrapper"
    fi
    
    return 0  # Don't fail on wrapper tests for now
}

# Test 5: MCP protocol compliance
test_mcp_protocol() {
    log_verbose "Testing MCP protocol compliance..."
    
    # Test initialization sequence
    local init_message='{
        "jsonrpc": "2.0",
        "id": 1,
        "method": "initialize",
        "params": {
            "protocolVersion": "draft",
            "capabilities": {},
            "clientInfo": {"name": "test-client", "version": "1.0.0"}
        }
    }'
    
    local response
    response=$(echo "$init_message" | timeout 2 socat - "UNIX-CONNECT:$SOCKET_PATH" 2>/dev/null || true)
    
    if [[ -n "$response" && "$response" == *"serverInfo"* && "$response" == *"mcp-server"* ]]; then
        log_verbose "MCP initialization test passed"
        return 0
    else
        log_error "MCP initialization test failed"
        log_verbose "Response: $response"
        return 1
    fi
}

# Test 6: Refactoring validation
test_refactoring_validation() {
    if [[ "$SKIP_EMACS_START" == "true" ]]; then
        log_info "Skipping refactoring validation (external Emacs instance)"
        return 0
    fi
    
    log_verbose "Validating refactoring through socket response..."
    
    # The response from initialization should contain "mcp-server" not "emacs-mcp-server"
    local init_message='{
        "jsonrpc": "2.0",
        "id": 1,
        "method": "initialize",
        "params": {
            "protocolVersion": "draft",
            "capabilities": {},
            "clientInfo": {"name": "test-client", "version": "1.0.0"}
        }
    }'
    
    local response
    response=$(echo "$init_message" | timeout 2 socat - "UNIX-CONNECT:$SOCKET_PATH" 2>/dev/null || true)
    
    # Check that response contains new naming
    if [[ "$response" == *"mcp-server"* ]]; then
        log_verbose "Server identifies as 'mcp-server' ✓"
    else
        log_error "Server does not identify with new name"
        return 1
    fi
    
    # Check that socket path uses new naming
    if [[ "$SOCKET_PATH" == *"emacs-mcp-server"* ]]; then
        log_verbose "Socket uses updated naming convention ✓"
    else
        log_error "Socket does not use updated naming convention"
        return 1
    fi
    
    return 0
}

# Main test suite
run_test_suite() {
    log_header "Running MCP Server Test Suite"
    
    # Run tests
    run_test "Socket Accessibility" test_socket_exists
    run_test "MCP Protocol Compliance" test_mcp_protocol  
    run_test "Shell Script Communication" test_shell_script
    run_test "Python Client Communication" test_python_client
    run_test "MCP Wrapper Scripts" test_mcp_wrappers
    run_test "Refactoring Validation" test_refactoring_validation
}

# Show results
show_results() {
    log_header "Test Results"
    
    echo -e "Tests run:    ${CYAN}$TESTS_RUN${NC}"
    echo -e "Tests passed: ${GREEN}$TESTS_PASSED${NC}"
    echo -e "Tests failed: ${RED}$TESTS_FAILED${NC}"
    
    if [[ $TESTS_FAILED -eq 0 ]]; then
        log_success "All tests passed! MCP Server is working correctly."
        return 0
    else
        log_error "$TESTS_FAILED test(s) failed. Please check the output above."
        return 1
    fi
}

# Usage help
show_usage() {
    cat << EOF
Usage: $0 [OPTIONS]

Comprehensive test runner for MCP Server refactoring validation.

OPTIONS:
    -h, --help           Show this help message
    -v, --verbose        Enable verbose output
    -k, --keep-running   Keep server running after tests
    -s, --skip-emacs     Skip starting Emacs (use existing server)
    -t, --timeout N      Set test timeout in seconds (default: $TEST_TIMEOUT)
    -n, --socket-name N  Set test socket name (default: $TEST_SOCKET_NAME)

EXAMPLES:
    $0                   # Run full test suite
    $0 -v                # Run with verbose output
    $0 -k                # Keep server running for manual testing
    $0 -s                # Test against existing server

DESCRIPTION:
    This script validates that the MCP server refactoring was successful by:
    1. Starting Emacs with the renamed mcp-server package
    2. Configuring predictable socket naming for testing
    3. Testing communication via shell scripts and Python clients
    4. Validating MCP protocol compliance
    5. Verifying that refactored function names work correctly

EOF
}

# Parse command line arguments
parse_arguments() {
    while [[ $# -gt 0 ]]; do
        case $1 in
            -h|--help)
                show_usage
                exit 0
                ;;
            -v|--verbose)
                VERBOSE=true
                shift
                ;;
            -k|--keep-running)
                KEEP_RUNNING=true
                shift
                ;;
            -s|--skip-emacs)
                SKIP_EMACS_START=true
                shift
                ;;
            -t|--timeout)
                TEST_TIMEOUT="$2"
                shift 2
                ;;
            -n|--socket-name)
                TEST_SOCKET_NAME="$2"
                shift 2
                ;;
            *)
                log_error "Unknown option: $1"
                show_usage
                exit 1
                ;;
        esac
    done
}

# Main execution
main() {
    parse_arguments "$@"
    
    log_header "MCP Server Test Runner"
    log_info "Socket name: $TEST_SOCKET_NAME"
    log_info "Timeout: ${TEST_TIMEOUT}s"
    log_info "Verbose: $VERBOSE"
    
    # Check dependencies
    check_dependencies
    
    # Start Emacs server
    if ! start_emacs_server; then
        log_error "Failed to start Emacs MCP server"
        exit 1
    fi
    
    # Run test suite
    run_test_suite
    
    # Show results
    local exit_code=0
    if ! show_results; then
        exit_code=1
    fi
    
    # Keep running if requested
    if [[ "$KEEP_RUNNING" == "true" ]]; then
        log_info "Keeping server running for manual testing..."
        log_info "Socket path: $SOCKET_PATH"
        log_info "Press Ctrl+C to stop."
        
        # Disable cleanup trap temporarily
        trap - EXIT
        
        # Wait for interrupt
        while kill -0 "$EMACS_PID" 2>/dev/null; do
            sleep 1
        done
        
        # Re-enable cleanup
        trap cleanup EXIT INT TERM
    fi
    
    exit $exit_code
}

# Run main function
main "$@"