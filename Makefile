.PHONY: test test-unit test-integration clean test-file test-pattern test-verbose help

# Default target
all: test

# Run all tests
test: test-unit test-integration

# Run unit tests
test-unit:
	emacs --batch -L . -L test/fixtures \
		-l test/unit/test-simple.el \
		-l test/unit/test-mcp-basic.el \
		-l test/unit/test-mcp-server-full.el \
		-l test/unit/test-mcp-tools-working.el \
		--eval "(ert-run-tests-batch-and-exit)"

# Run integration tests
test-integration:
	./test/scripts/test-runner.sh

# Run tests with verbose output
test-verbose:
	emacs --batch -L . -L test/fixtures \
		-l test/unit/test-simple.el \
		-l test/unit/test-mcp-basic.el \
		-l test/unit/test-mcp-server-full.el \
		-l test/unit/test-mcp-tools-working.el \
		--eval "(let ((ert-batch-backtrace-right-margin 80)) (ert-run-tests-batch-and-exit t))"

# Clean up temporary files
clean:
	rm -f *.elc test/**/*.elc
	rm -rf /tmp/emacs-mcp-server-test

# Run specific test file
test-file:
	@if [ -z "$(FILE)" ]; then echo "Usage: make test-file FILE=test/unit/test-filename.el"; exit 1; fi
	emacs --batch -L . -L test/fixtures -l $(FILE) --eval "(ert-run-tests-batch-and-exit)"

# Run tests matching pattern
test-pattern:
	@if [ -z "$(PATTERN)" ]; then echo "Usage: make test-pattern PATTERN=pattern"; exit 1; fi
	emacs --batch -L . -L test/fixtures \
		-l test/unit/test-simple.el \
		-l test/unit/test-mcp-basic.el \
		-l test/unit/test-mcp-server-full.el \
		-l test/unit/test-mcp-tools-working.el \
		--eval "(ert-run-tests-batch-and-exit \"$(PATTERN)\")"

# Show help
help:
	@echo "Emacs MCP Server Test Suite"
	@echo "==========================="
	@echo ""
	@echo "Available targets:"
	@echo "  test             - Run all tests (unit + integration)"
	@echo "  test-unit        - Run unit tests (39 ERT tests)"
	@echo "  test-integration - Run integration tests (6 tests via test-runner.sh)"
	@echo "  test-verbose     - Run unit tests with verbose output"
	@echo "  test-pattern PATTERN=<pattern> - Run tests matching pattern"
	@echo "  test-file FILE=<file>          - Run specific test file"
	@echo "  clean            - Clean up compiled files and test artifacts"
	@echo "  help             - Show this help message"
	@echo ""
	@echo "Examples:"
	@echo "  make test"
	@echo "  make test-pattern PATTERN=jsonrpc"
	@echo "  make test-file FILE=test/unit/test-mcp-server-full.el"
