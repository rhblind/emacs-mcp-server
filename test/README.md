# MCP Server Test Suite

This directory contains a comprehensive test suite for the Emacs MCP Server implementation using ERT (Emacs Regression Testing).

## Test Structure

```
test/
├── unit/                    # Unit tests for individual modules
│   ├── test-mcp-protocol.el    # MCP protocol and JSON-RPC tests
│   ├── test-mcp-security.el    # Security layer tests
│   ├── test-mcp-transport.el   # Transport layer tests
│   ├── test-mcp-tools.el       # Tool registry tests
│   └── test-mcp-user-functions.el  # User-facing function tests
├── integration/             # Integration tests (existing)
├── fixtures/                # Test utilities and helpers
│   ├── test-helpers.el         # Common test utilities
│   └── test-coverage.el        # Code coverage setup
├── data/                    # Test data files
│   ├── sample-mcp-messages.json   # Sample MCP messages
│   ├── test-config.json           # Test configurations
│   └── sample-elisp-expressions.el # Sample expressions for testing
└── README.md               # This file
```

## Running Tests

### Prerequisites

Install basic test dependencies (optional):

```bash
make install-deps
```

No external package manager required - uses Emacs built-in testing with ERT.

### Running All Tests

```bash
make test
```

### Running Specific Test Categories

```bash
# Basic unit tests (simple infrastructure tests)
make test-unit

# All unit tests (including MCP-specific ones)
make test-unit-all

# Integration tests only  
make test-integration

# Tests with verbose output
make test-verbose

# Specific test file
make test-file FILE=test/unit/test-simple.el

# Tests matching pattern
make test-pattern PATTERN=simple
```

### Running Tests with Coverage

Basic coverage tracking is included. Advanced coverage requires installing `undercover.el` package separately.

## Test Categories

### Unit Tests

#### MCP Protocol Tests (`test-mcp-protocol.el`)
- JSON-RPC message parsing and validation
- MCP protocol compliance (initialize, tools/list, tools/call)
- Error handling for malformed messages
- Message serialization/deserialization

#### Security Layer Tests (`test-mcp-security.el`)
- Permission system (dangerous functions, file patterns)
- Input validation and sanitization
- Audit logging functionality
- Safe evaluation with timeouts and memory limits
- Security prompt mocking

#### Transport Layer Tests (`test-mcp-transport.el`)
- Unix socket creation and cleanup
- Multi-client connection handling
- Message buffering and fragmentation
- Socket naming strategies
- Conflict resolution

#### Tool Registry Tests (`test-mcp-tools.el`)
- Tool registration and listing
- Input schema validation
- Tool execution and error handling
- Tool metadata management

#### User Functions Tests (`test-mcp-user-functions.el`)
- Interactive commands (start, stop, status, etc.)
- Configuration options and customization
- Debug and logging functions
- Client management

### Integration Tests

Integration tests are located in `test/integration/` and test complete workflows including:
- Server startup and shutdown
- Client connections
- End-to-end message processing

## Test Utilities

### Test Helper Functions (`test/fixtures/test-helpers.el`)

Common utilities for all tests:

- **Fixture Management**: `mcp-test-with-temp-dir`, `mcp-test-with-temp-socket`
- **Mock Data**: JSON-RPC message builders, mock client simulator
- **Security Testing**: Permission response mocking, sensitive file creation
- **Assertions**: Validation helpers for MCP-specific data structures

### Coverage Tracking (`test/fixtures/test-coverage.el`)

Automated code coverage using undercover.el:

- Tracks coverage for all source files
- Generates LCOV reports
- CI integration with Coveralls
- Coverage threshold checking (85% minimum)

## Test Data

### Sample Messages (`test/data/sample-mcp-messages.json`)
- Valid MCP requests and responses
- Error responses
- Malformed messages for error testing
- Security test inputs

### Test Configurations (`test/data/test-config.json`)
- Server configurations for different test scenarios
- Client capability configurations
- Transport settings

### Sample Expressions (`test/data/sample-elisp-expressions.el`)
- Safe expressions for basic testing
- Dangerous expressions requiring security checks
- Complex expressions for advanced scenarios
- Malformed expressions for error testing

## Writing New Tests

### Basic Test Structure

```elisp
(ert-deftest my-test-name ()
  "Test description."
  (mcp-test-with-mock-server
   ;; Test setup
   (should (equal (my-function "input") "expected-output"))
   ;; Test assertions
   (should-not (my-function nil))))
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

## Continuous Integration

Tests run automatically on:
- Push to main/develop branches
- Pull requests
- Multiple Emacs versions (28.1, 28.2, 29.1, snapshot)

Coverage reports are uploaded to Coveralls for coverage tracking over time.

## Coverage Goals

- **Minimum Coverage**: 85% line coverage
- **Focus Areas**: Security layer, protocol handling, transport layer
- **Exclusions**: Test files, external dependencies

## Best Practices

1. **Test Isolation**: Each test should be independent and clean up after itself
2. **Mock External Dependencies**: Use mocking for file system, network, user input
3. **Test Error Conditions**: Include tests for error cases and edge conditions
4. **Clear Test Names**: Use descriptive test names that explain what is being tested
5. **Comprehensive Assertions**: Test both positive and negative cases
6. **Performance Awareness**: Keep tests fast, use timeouts for long-running operations