# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.6.0](https://github.com/rhblind/emacs-mcp-server/compare/v0.5.0...v0.6.0) (2026-03-31)

### Fixed
- `mcp-server-security--is-sensitive-file`: patterns using `~/` prefix (e.g. `"~/.ssh/"`) were never matched because the input path was expanded with `expand-file-name` but the patterns were not, causing the literal `~` to fail against an absolute path (issue #9)
- `mcp-server-security--is-dangerous-operation`: calling `symbol-name` on a string operation ID (e.g. `"access-sensitive-file:find-file"`) raised `wrong-type-argument: symbolp` instead of returning a result
- Sensitive file check was only applied to four functions (`find-file`, `find-file-noselect`, `view-file`, `insert-file-contents`); functions like `copy-file` and `rename-file` could still access sensitive files when placed in `mcp-server-security-allowed-dangerous-functions`

### Added
- Glob pattern support (`*`, `?`) in `mcp-server-security-sensitive-file-patterns`: patterns such as `"~/.authinfo*"` now correctly match all variants (`~/.authinfo.gpg`, `~/.authinfo.enc`, etc.) via `wildcard-to-regexp`
- Sensitive file check now covers `copy-file` (args 1 and 2), `rename-file` (args 1 and 2), `write-region` (arg 3), `append-to-file` (arg 3), `write-file` (arg 1), and `insert-file-contents-literally` (arg 1), blocking both reads from and writes to sensitive paths
- New entries in default `mcp-server-security-dangerous-functions`: `append-to-file`, `async-shell-command`, `directory-files`, `directory-files-recursively`, `insert-file-contents-literally`, `make-network-process`, `make-process`, `open-network-stream`, `setenv`, `with-temp-file`, `write-file`
- Removed entries from default `mcp-server-security-dangerous-functions` that were either non-functional or overly broad: `process-environment` (a variable, not a function; the form walker never matched it), `shell-environment` (not a standard Emacs function), `require` (too restrictive for legitimate use), `save-current-buffer` and `set-buffer` (low-level context-switching primitives with no inherent danger), `switch-to-buffer` (interactive UI command with no security impact)
- Comprehensive unit test suite for `mcp-server-security` (79 ERT tests total)
- Known limitation documented and tracked: static form walker does not recurse into `let`-binding value positions and cannot detect dynamically-constructed function names (`funcall` + `intern`); see issue #10

## [0.4.0](https://github.com/rhblind/emacs-mcp-server/compare/v0.3.0...v0.4.0) (2026-01-08)

### Added
- `get-diagnostics` tool for flycheck/flymake error reporting
- Modular tool system with self-registering plugins in `tools/` directory
- Selective tool enabling via `mcp-server-emacs-tools-enabled`
- Runtime tool filtering via `mcp-server-tools-filter` predicate
- Automatic server shutdown when Unix socket terminates unexpectedly
- Comprehensive Elisp code conventions in CLAUDE.md
- Security limitations section in README documenting blocklist bypass methods

### Changed
- Lowered minimum Emacs version from 28.1 to 27.1 (native JSON available since 27.1)
- Tools now use self-registration pattern (register on `require`)
- Converted `mcp-server-debug` and `mcp-server-default-transport` to `defcustom`
- Converted security timeouts to `defcustom` for user configuration
- Replaced `sleep-for` with `sit-for` for proper event handling in main loop
- Improved tool cleanup to preserve definitions (only resets runtime state)
- Refactored tool system into modular architecture

### Fixed
- Version mismatch between package header and `mcp-server-version` defconst
- Abstraction violation: transport now uses public `mcp-server-transport-send-raw` API
- Load-path setup for missing file variables
- Removed redundant runtime `require` statement
- Removed backward compatibility aliases causing defvar/defcustom confusion
- Removed autoload cookie from internal `mcp-server-main` function

### Removed
- Backward compatibility variable aliases in security module

## [0.3.0](https://github.com/rhblind/emacs-mcp-server/compare/v0.2.0...v0.3.0) (2026-01-07)

### Added
- Granular permission prompts with session management
- GNU General Public License v3

### Fixed
- Test runner with safeguards and retries for socket operations

## [0.2.0](https://github.com/rhblind/emacs-mcp-server/compare/v0.1.0...v0.2.0) (2026-01-06)

### Added
- Enhanced sensitive file and function protection
- Repository badges for tests, license, and Emacs versions
- Emacs 30.x to CI test matrix

### Changed
- Improved documentation with socat requirement note

## [0.1.0](https://github.com/rhblind/emacs-mcp-server/releases/tag/v0.1.0) (2026-01-01)

### Added
- Initial MCP server implementation in pure Elisp
- Unix domain socket transport
- `eval-elisp` tool for executing arbitrary Elisp expressions
- Security sandbox with dangerous function blocklist
- Permission caching per session
- Multi-client support with independent connection tracking
- Python and shell wrapper scripts for MCP client integration
- Comprehensive test suite
- Demo images for theme change and poem writing

