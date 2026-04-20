# MCP Server Test Suite

This directory contains the test suite for the Emacs MCP Server implementation.

## Running Tests

### Run All Tests

```bash
make test
```

This runs both unit tests (79 ERT tests) and integration tests (6 tests).

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
│   ├── test-mcp-emacs-tools.el    # eval-elisp and get-diagnostics tool tests
│   ├── test-mcp-security.el       # Security module tests (sensitive files, permissions)
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

### test-mcp-emacs-tools.el
- eval-elisp tool: expression evaluation, error handling
- get-diagnostics tool: structure, severity filtering, sorting
- Tool registration and filtering

### test-mcp-security.el
Tests for `mcp-server-security`:
- `mcp-server-security--is-sensitive-file`: pattern matching for `~/`-prefixed,
  absolute-path, and bare-filename patterns; allowed-files bypass; nil/non-string inputs
- `mcp-server-security--is-dangerous-operation`: symbol operations, allowed-list bypass,
  name-pattern matching, string operation IDs (regression: `symbol-name` on string)
- `mcp-server-security--check-form-safety`: blocks dangerous functions, allows safe
  functions, blocks sensitive file access even when function is in the allowed list,
  recursive argument checking
- Issue #9 regression: `eval-elisp` must block `(find-file "~/.ssh/id_rsa")` even
  when `find-file` is in `mcp-server-security-allowed-dangerous-functions`
- Glob patterns: `~/.authinfo*` matches all `.authinfo` variants via `wildcard-to-regexp`
- New dangerous functions: `with-temp-file`, `write-file`, `append-to-file`,
  `make-network-process`, `open-network-stream`, `directory-files`,
  `directory-files-recursively`, `insert-file-contents-literally`
- Sensitive file check for multi-arg functions: `copy-file` (src + dst),
  `rename-file` (src + dst), `write-region` (arg 3), `write-file` (arg 1)
- Permission caching: storage, cache hits, clear
- Input validation: shell metacharacters, dangerous elisp patterns, length limit
- Audit logging: event recording, denial logging, 1000-entry cap

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

## Org Tools Tests

Unit tests for the org-mode tools live in `test/unit/`:

- `test-mcp-org-common.el` - node resolution, serialization, path validation, helper macros.
- `test-mcp-org-helpers.el` - the `mcp-test-with-org-fixture` macro itself.
- `test-mcp-org-get-node.el`, `test-mcp-org-search.el`, `test-mcp-org-agenda.el`, `test-mcp-org-list-templates.el`, `test-mcp-org-list-tags.el` - read tools.
- `test-mcp-org-capture.el`, `test-mcp-org-update-node.el`, `test-mcp-org-refile.el`, `test-mcp-org-archive.el`, `test-mcp-org-clock.el` - write tools.
- `test-mcp-org-roam-search.el`, `test-mcp-org-roam-get-node.el`, `test-mcp-org-roam-capture.el` - org-roam tools; skipped gracefully when `org-roam` is not installed.
- `test-mcp-org-registry.el` - verifies org tools appear in the registry and respect the `mcp-server-emacs-tools-enabled` filter.

Fixtures live in `test/fixtures/org/`:

- `sample-notes.org` - for outline-path, property, and body tests.
- `sample-agenda.org` - TODO/NEXT/DONE states, tags, schedules, deadlines.

Tests use the `mcp-test-with-org-fixture` macro (in `test/fixtures/test-helpers.el`) which copies a fixture to a temp file, isolates `org-id-locations`, and scopes `mcp-server-emacs-tools-org-allowed-roots` per-test.

Run any single suite with:

```bash
emacs -batch -L . -L test/fixtures -L test/unit -L tools \
  -l ert -l test/unit/test-mcp-org-<suite>.el \
  -f ert-run-tests-batch-and-exit
```
