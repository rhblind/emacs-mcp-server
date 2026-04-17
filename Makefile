.PHONY: test test-unit test-integration clean test-file test-pattern test-verbose install-deps help

# Default target
all: test

# Run all tests
test: test-unit test-integration

# Run unit tests
test-unit:
	emacs --batch -L . -L tools -L test/fixtures \
		-l test/fixtures/bootstrap-elpa.el \
		-l test/unit/test-simple.el \
		-l test/unit/test-mcp-basic.el \
		-l test/unit/test-mcp-server-full.el \
		-l test/unit/test-mcp-tools-working.el \
		-l test/unit/test-mcp-emacs-tools.el \
		-l test/unit/test-mcp-security.el \
		-l test/unit/test-mcp-org-helpers.el \
		-l test/unit/test-mcp-org-common.el \
		-l test/unit/test-mcp-org-get-node.el \
		-l test/unit/test-mcp-org-search.el \
		-l test/unit/test-mcp-org-agenda.el \
		-l test/unit/test-mcp-org-list-templates.el \
		-l test/unit/test-mcp-org-list-tags.el \
		-l test/unit/test-mcp-org-capture.el \
		-l test/unit/test-mcp-org-update-node.el \
		-l test/unit/test-mcp-org-refile.el \
		-l test/unit/test-mcp-org-archive.el \
		-l test/unit/test-mcp-org-clock.el \
		-l test/unit/test-mcp-org-roam-search.el \
		-l test/unit/test-mcp-org-roam-get-node.el \
		-l test/unit/test-mcp-org-roam-capture.el \
		-l test/unit/test-mcp-org-registry.el \
		--eval "(ert-run-tests-batch-and-exit)"

# Run integration tests
test-integration:
	./test/scripts/test-runner.sh

# Run tests with verbose output
test-verbose:
	emacs --batch -L . -L tools -L test/fixtures \
		-l test/fixtures/bootstrap-elpa.el \
		-l test/unit/test-simple.el \
		-l test/unit/test-mcp-basic.el \
		-l test/unit/test-mcp-server-full.el \
		-l test/unit/test-mcp-tools-working.el \
		-l test/unit/test-mcp-emacs-tools.el \
		-l test/unit/test-mcp-security.el \
		-l test/unit/test-mcp-org-helpers.el \
		-l test/unit/test-mcp-org-common.el \
		-l test/unit/test-mcp-org-get-node.el \
		-l test/unit/test-mcp-org-search.el \
		-l test/unit/test-mcp-org-agenda.el \
		-l test/unit/test-mcp-org-list-templates.el \
		-l test/unit/test-mcp-org-list-tags.el \
		-l test/unit/test-mcp-org-capture.el \
		-l test/unit/test-mcp-org-update-node.el \
		-l test/unit/test-mcp-org-refile.el \
		-l test/unit/test-mcp-org-archive.el \
		-l test/unit/test-mcp-org-clock.el \
		-l test/unit/test-mcp-org-roam-search.el \
		-l test/unit/test-mcp-org-roam-get-node.el \
		-l test/unit/test-mcp-org-roam-capture.el \
		-l test/unit/test-mcp-org-registry.el \
		--eval "(let ((ert-batch-backtrace-right-margin 80)) (ert-run-tests-batch-and-exit t))"

# Clean up temporary files
clean:
	rm -f *.elc test/**/*.elc
	rm -rf /tmp/emacs-mcp-server-test

# Run specific test file
test-file:
	@if [ -z "$(FILE)" ]; then echo "Usage: make test-file FILE=test/unit/test-filename.el"; exit 1; fi
	emacs --batch -L . -L tools -L test/fixtures \
		-l test/fixtures/bootstrap-elpa.el \
		-l $(FILE) --eval "(ert-run-tests-batch-and-exit)"

# Run tests matching pattern
test-pattern:
	@if [ -z "$(PATTERN)" ]; then echo "Usage: make test-pattern PATTERN=pattern"; exit 1; fi
	emacs --batch -L . -L tools -L test/fixtures \
		-l test/fixtures/bootstrap-elpa.el \
		-l test/unit/test-simple.el \
		-l test/unit/test-mcp-basic.el \
		-l test/unit/test-mcp-server-full.el \
		-l test/unit/test-mcp-tools-working.el \
		-l test/unit/test-mcp-emacs-tools.el \
		-l test/unit/test-mcp-security.el \
		-l test/unit/test-mcp-org-helpers.el \
		-l test/unit/test-mcp-org-common.el \
		-l test/unit/test-mcp-org-get-node.el \
		-l test/unit/test-mcp-org-search.el \
		-l test/unit/test-mcp-org-agenda.el \
		-l test/unit/test-mcp-org-list-templates.el \
		-l test/unit/test-mcp-org-list-tags.el \
		-l test/unit/test-mcp-org-capture.el \
		-l test/unit/test-mcp-org-update-node.el \
		-l test/unit/test-mcp-org-refile.el \
		-l test/unit/test-mcp-org-archive.el \
		-l test/unit/test-mcp-org-clock.el \
		-l test/unit/test-mcp-org-roam-search.el \
		-l test/unit/test-mcp-org-roam-get-node.el \
		-l test/unit/test-mcp-org-roam-capture.el \
		-l test/unit/test-mcp-org-registry.el \
		--eval "(ert-run-tests-batch-and-exit \"$(PATTERN)\")"

# Install test dependencies.
# On Emacs 29+, install org-roam into test/.elpa/ so the roam tool tests
# can exercise real handlers.  On Emacs 28.x, skip roam install (the roam
# tests are version-gated and register-only on that version).
install-deps:
	@emacs --batch --eval "(progn \
	  (if (>= emacs-major-version 29) \
	      (progn \
	        (require 'package) \
	        (setq package-user-dir (expand-file-name \"test/.elpa\" default-directory)) \
	        (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t) \
	        (package-initialize) \
	        (unless (package-installed-p 'org-roam) \
	          (message \"Installing org-roam into %s ...\" package-user-dir) \
	          (package-refresh-contents) \
	          (package-install 'org-roam) \
	          (message \"org-roam installed.\")) \
	        (message \"Test dependencies ready (Emacs %s, org-roam present).\" emacs-version)) \
	    (message \"Emacs %s: skipping org-roam install (requires 29+).\" emacs-version)))"

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
