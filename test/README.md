# MCP Server Test Suite

This directory contains the complete test suite for the Emacs MCP Server implementation.

## Directory Structure

### `config/`
Test configuration files and utilities:
- `test-config.el` - Main test configuration with server setup functions
- `minimal-test-config.el` - Minimal configuration for lightweight testing
- `test-json-false.el` - JSON serialization and boolean handling tests

### `scripts/`
Test runner scripts and utilities:
- `test-runner.sh` - Main comprehensive test suite runner
- `start-test-server.sh` - Utility to start test server instances
- `test-hello-world.sh` - Basic "hello world" functionality test
- `test-hello-world.py` - Python-based hello world test
- `debug-test.py` - Debug utilities for troubleshooting

### `integration/`
Integration test scripts for end-to-end testing:
- `test-unix-socket-fixed.sh` - Unix domain socket communication tests
- `test-unix-socket.sh` - Original socket tests (for comparison)

## Usage

### Running the Full Test Suite
```bash
./test/scripts/test-runner.sh
```

### Running with Options
```bash
./test/scripts/test-runner.sh -v          # Verbose output
./test/scripts/test-runner.sh -k          # Keep server running after tests
./test/scripts/test-runner.sh -s          # Test against existing server
```

### Running Individual Tests
```bash
./test/integration/test-unix-socket-fixed.sh   # Socket communication test
./test/scripts/test-hello-world.sh             # Basic functionality test
```

## Test Configuration

The test suite uses predictable socket naming and isolated configurations to ensure reproducible results. All tests are designed to clean up after themselves unless explicitly requested to keep resources running.

For detailed information about specific tests, see the individual script files or run with the `-h` flag where supported.