# MCP Server Test Suite

This directory contains the test suite for the Emacs MCP Server implementation.

## Running Tests

### Run All Tests

```bash
make test
```

This runs both unit tests (39 ERT tests) and integration tests (6 tests).

### Run Specific Test Categories

```bash
# Unit tests only
make test-unit

# Integration tests only
make test-integration

# Unit tests with verbose output
make test-verbose

# Specific test file
make test-file FILE=test/unit/test-simple.el

# Tests matching pattern
make test-pattern PATTERN=jsonrpc
```

## Test Structure

```
test/
├── unit/                    # ERT unit tests
│   ├── test-simple.el       # Basic infrastructure tests
│   ├── test-mcp-basic.el    # JSON-RPC and protocol pattern tests
│   ├── test-mcp-server-full.el    # Server integration tests
│   └── test-mcp-tools-working.el  # Tool registry tests
├── integration/             # Integration tests
│   └── test-unix-socket-fixed.sh  # Unix socket communication tests
├── scripts/                 # Test runner scripts
│   ├── test-runner.sh       # Main integration test runner
│   ├── test-hello-world.py  # Python client test
│   ├── test-hello-world.sh  # Shell client test
│   └── start-test-server.sh # Server startup helper
├── config/                  # Test configuration
│   └── test-config.el       # Test server configuration
└── fixtures/                # Test utilities
    └── test-helpers.el      # Common test helpers and macros
```

## Unit Tests

### test-simple.el
Basic infrastructure tests to verify the test framework works.

### test-mcp-basic.el
- JSON-RPC message structure validation
- Protocol version format
- Server capabilities structure
- Security patterns (dangerous functions, sensitive files)
- Tool schema structure
- Socket naming patterns
- Message buffering patterns

### test-mcp-server-full.el
- Server module loading
- Server state management
- Debug toggle functionality
- Socket configuration

### test-mcp-tools-working.el
- Tool registration
- Tool listing
- Tool retrieval
- Tool execution

## Integration Tests

The integration test suite (`test-runner.sh`) starts an actual Emacs MCP server and tests:

1. **Socket Accessibility** - Verifies the Unix socket is created and accessible
2. **MCP Protocol Compliance** - Sends initialize request, verifies response
3. **Shell Script Communication** - Tests via shell scripts
4. **Python Client Communication** - Tests via Python client
5. **MCP Wrapper Scripts** - Tests wrapper script functionality
6. **Refactoring Validation** - Verifies server naming conventions

## Test Helpers

The `test-helpers.el` file provides:

- `mcp-test-with-temp-dir` - Creates temporary directory for file tests
- `mcp-test-with-mock-server` - Sets up mock server environment
- `mcp-test-with-permission-responses` - Mocks security permission prompts
- JSON-RPC message builders and validators

## Writing New Tests

### Basic Test Structure

```elisp
(ert-deftest my-test-name ()
  "Test description."
  (should (equal (my-function "input") "expected-output"))
  (should-not (my-function nil)))
```

### Using Test Fixtures

```elisp
(ert-deftest my-file-test ()
  "Test file operations."
  (mcp-test-with-temp-dir
   (let ((test-file (expand-file-name "test.txt" mcp-test-temp-dir)))
     (with-temp-file test-file
       (insert "test content"))
     (should (file-exists-p test-file)))))
```

### Mocking Security Prompts

```elisp
(ert-deftest my-security-test ()
  "Test security prompts."
  (mcp-test-with-permission-responses '(t nil t)
   ;; First prompt returns t (granted)
   ;; Second prompt returns nil (denied)
   ;; Third prompt returns t (granted)
   (should (mcp-server-security-check-permission 'dangerous-function))))
```

## Test Configuration

Integration tests use a dedicated socket directory (`/tmp/emacs-mcp-server-test`) to avoid conflicts with running servers. The socket name defaults to `test-instance`.

## Best Practices

1. **Test Isolation** - Each test should be independent and clean up after itself
2. **Mock External Dependencies** - Use mocking for user input prompts
3. **Test Error Conditions** - Include tests for error cases
4. **Clear Test Names** - Use descriptive names that explain what is being tested
5. **MCP Spec Compliance** - JSON messages must be single-line (no embedded newlines)
