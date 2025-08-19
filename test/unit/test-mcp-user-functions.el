;;; test-mcp-user-functions.el --- Tests for User-Facing Functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for all interactive commands, configuration options,
;; and user-exposed functionality.

;;; Code:

(require 'ert)
(require 'test-helpers)

;;; Server Start/Stop Tests

(ert-deftest mcp-test-server-start-basic ()
  "Test basic server start functionality."
  (let ((mcp-server-running nil)
        (mcp-server-current-transport nil))
    (with-mock
      (mock (mcp-server-transport-start * *) => t)
      (mock (mcp-server-tools-initialize))
      (mcp-server-start)
      (should mcp-server-running)
      (should (equal mcp-server-current-transport "unix")))))

(ert-deftest mcp-test-server-start-with-debug ()
  "Test server start with debug enabled."
  (let ((mcp-server-running nil)
        (mcp-server-debug nil))
    (with-mock
      (mock (mcp-server-transport-start * *) => t)
      (mock (mcp-server-tools-initialize))
      (mcp-server-start t)
      (should mcp-server-debug))))

(ert-deftest mcp-test-server-start-already-running ()
  "Test server start when already running."
  (let ((mcp-server-running t))
    (should-error (mcp-server-start))))

(ert-deftest mcp-test-server-start-unix ()
  "Test Unix-specific server start."
  (let ((mcp-server-running nil))
    (with-mock
      (mock (mcp-server-transport-start "unix" *) => t)
      (mock (mcp-server-tools-initialize))
      (mcp-server-start-unix)
      (should (equal mcp-server-current-transport "unix")))))

(ert-deftest mcp-test-server-start-tcp ()
  "Test TCP-specific server start."
  (let ((mcp-server-running nil))
    (with-mock
      (mock (mcp-server-transport-start "tcp" *) => t)
      (mock (mcp-server-tools-initialize))
      (mcp-server-start-tcp)
      (should (equal mcp-server-current-transport "tcp")))))

(ert-deftest mcp-test-server-stop ()
  "Test server stop functionality."
  (let ((mcp-server-running t)
        (mcp-server-current-transport "unix"))
    (with-mock
      (mock (mcp-server-transport-stop "unix"))
      (mcp-server-stop)
      (should-not mcp-server-running)
      (should-not mcp-server-current-transport))))

(ert-deftest mcp-test-server-stop-not-running ()
  "Test server stop when not running."
  (let ((mcp-server-running nil))
    (should-error (mcp-server-stop))))

(ert-deftest mcp-test-server-restart ()
  "Test server restart functionality."
  (let ((mcp-server-running t)
        (mcp-server-current-transport "unix"))
    (with-mock
      (mock (mcp-server-transport-stop "unix"))
      (mock (mcp-server-transport-start "unix" *) => t)
      (mock (mcp-server-tools-initialize))
      (mcp-server-restart)
      (should mcp-server-running))))

;;; Status and Information Tests

(ert-deftest mcp-test-server-status-running ()
  "Test server status when running."
  (let ((mcp-server-running t)
        (mcp-server-current-transport "unix")
        (mcp-server-debug t))
    (with-mock
      (mock (mcp-server-transport-unix-get-socket-path) => "/tmp/test.sock")
      (mock (mcp-server-transport-unix-list-clients) => '("client1" "client2"))
      (let ((status (mcp-server-status)))
        (should (alist-get 'running status))
        (should (equal (alist-get 'transport status) "unix"))
        (should (alist-get 'debug status))
        (should (equal (alist-get 'socket-path status) "/tmp/test.sock"))
        (should (= (length (alist-get 'clients status)) 2))))))

(ert-deftest mcp-test-server-status-not-running ()
  "Test server status when not running."
  (let ((mcp-server-running nil))
    (let ((status (mcp-server-status)))
      (should-not (alist-get 'running status))
      (should-not (alist-get 'transport status))
      (should-not (alist-get 'socket-path status))
      (should (= (length (alist-get 'clients status)) 0)))))

(ert-deftest mcp-test-server-is-running-p ()
  "Test server running predicate."
  (let ((mcp-server-running t))
    (should (mcp-server-is-running-p)))
  (let ((mcp-server-running nil))
    (should-not (mcp-server-is-running-p))))

(ert-deftest mcp-test-server-get-version ()
  "Test server version retrieval."
  (should (stringp (mcp-server-get-version)))
  (should (string-match-p "^[0-9]+\\.[0-9]+\\.[0-9]+$" (mcp-server-get-version))))

(ert-deftest mcp-test-server-get-protocol-version ()
  "Test protocol version retrieval."
  (should (stringp (mcp-server-get-protocol-version)))
  (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" 
                          (mcp-server-get-protocol-version))))

;;; Socket Management Tests

(ert-deftest mcp-test-server-get-socket-path ()
  "Test socket path retrieval."
  (let ((mcp-server-current-transport "unix"))
    (with-mock
      (mock (mcp-server-transport-unix-get-socket-path) => "/tmp/test.sock")
      (should (equal (mcp-server-get-socket-path) "/tmp/test.sock")))))

(ert-deftest mcp-test-server-get-socket-path-not-unix ()
  "Test socket path retrieval for non-Unix transport."
  (let ((mcp-server-current-transport "tcp"))
    (should-not (mcp-server-get-socket-path))))

(ert-deftest mcp-test-server-set-socket-name ()
  "Test socket name configuration."
  (let ((original mcp-server-socket-name))
    (unwind-protect
        (progn
          (mcp-server-set-socket-name "custom-name")
          (should (equal mcp-server-socket-name "custom-name"))
          (mcp-server-set-socket-name 'user)
          (should (equal mcp-server-socket-name 'user))
          (mcp-server-set-socket-name nil)
          (should-not mcp-server-socket-name))
      (setq mcp-server-socket-name original))))

;;; Debug and Logging Tests

(ert-deftest mcp-test-server-toggle-debug ()
  "Test debug mode toggle."
  (let ((mcp-server-debug nil))
    (mcp-server-toggle-debug)
    (should mcp-server-debug)
    (mcp-server-toggle-debug)
    (should-not mcp-server-debug)))

(ert-deftest mcp-test-server-enable-debug ()
  "Test debug mode enable."
  (let ((mcp-server-debug nil))
    (mcp-server-enable-debug)
    (should mcp-server-debug)))

(ert-deftest mcp-test-server-disable-debug ()
  "Test debug mode disable."
  (let ((mcp-server-debug t))
    (mcp-server-disable-debug)
    (should-not mcp-server-debug)))

(ert-deftest mcp-test-server-logging-when-enabled ()
  "Test that logging works when debug is enabled."
  (let ((mcp-server-debug t)
        (logged-messages '()))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) logged-messages))))
      (mcp-server--debug "Test message: %s" "value")
      (should (string-match-p "\\[MCP DEBUG\\].*Test message: value"
                              (car logged-messages))))))

(ert-deftest mcp-test-server-no-logging-when-disabled ()
  "Test that logging is disabled when debug is off."
  (let ((mcp-server-debug nil)
        (logged-messages '()))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) logged-messages))))
      (mcp-server--debug "Test message")
      (should-not logged-messages))))

;;; Client Management Tests

(ert-deftest mcp-test-server-list-clients ()
  "Test client listing."
  (let ((mcp-server-current-transport "unix"))
    (with-mock
      (mock (mcp-server-transport-unix-list-clients) => '("client1" "client2" "client3"))
      (let ((clients (mcp-server-list-clients)))
        (should (= (length clients) 3))
        (should (member "client1" clients))))))

(ert-deftest mcp-test-server-list-clients-no-transport ()
  "Test client listing with no transport."
  (let ((mcp-server-current-transport nil))
    (should (= (length (mcp-server-list-clients)) 0))))

(ert-deftest mcp-test-server-disconnect-client ()
  "Test client disconnection."
  (let ((mcp-server-current-transport "unix"))
    (with-mock
      (mock (mcp-server-transport-unix-disconnect-client "client1"))
      (should-not (mcp-server-disconnect-client "client1")))))

;;; Configuration Tests

(ert-deftest mcp-test-server-socket-directory-config ()
  "Test socket directory configuration."
  (let ((original mcp-server-socket-directory))
    (unwind-protect
        (progn
          (setq mcp-server-socket-directory "/tmp/custom/")
          (should (equal mcp-server-socket-directory "/tmp/custom/")))
      (setq mcp-server-socket-directory original))))

(ert-deftest mcp-test-server-conflict-resolution-config ()
  "Test socket conflict resolution configuration."
  (let ((original mcp-server-socket-conflict-resolution))
    (unwind-protect
        (progn
          (setq mcp-server-socket-conflict-resolution 'error)
          (should (equal mcp-server-socket-conflict-resolution 'error))
          (setq mcp-server-socket-conflict-resolution 'auto)
          (should (equal mcp-server-socket-conflict-resolution 'auto)))
      (setq mcp-server-socket-conflict-resolution original))))

(ert-deftest mcp-test-server-default-transport-config ()
  "Test default transport configuration."
  (let ((original mcp-server-default-transport))
    (unwind-protect
        (progn
          (setq mcp-server-default-transport "tcp")
          (should (equal mcp-server-default-transport "tcp")))
      (setq mcp-server-default-transport original))))

;;; Capabilities Tests

(ert-deftest mcp-test-server-capabilities-structure ()
  "Test server capabilities structure."
  (should (alist-get 'tools mcp-server-capabilities))
  (should (alist-get 'resources mcp-server-capabilities))
  (should (alist-get 'prompts mcp-server-capabilities))
  (should (alist-get 'listChanged (alist-get 'tools mcp-server-capabilities))))

(ert-deftest mcp-test-server-get-capabilities ()
  "Test capabilities retrieval."
  (let ((caps (mcp-server-get-capabilities)))
    (should (equal caps mcp-server-capabilities))))

;;; Error Handling Tests

(ert-deftest mcp-test-server-start-transport-failure ()
  "Test server start with transport failure."
  (let ((mcp-server-running nil))
    (with-mock
      (mock (mcp-server-transport-start * *) ==> (error "Transport failed"))
      (should-error (mcp-server-start))
      (should-not mcp-server-running))))

(ert-deftest mcp-test-server-graceful-shutdown ()
  "Test graceful server shutdown on error."
  (let ((mcp-server-running t)
        (mcp-server-current-transport "unix"))
    (with-mock
      (mock (mcp-server-transport-stop "unix"))
      (condition-case err
          (error "Simulated error")
        (error 
         (mcp-server-stop)
         (should-not mcp-server-running))))))

;;; Interactive Command Tests

(ert-deftest mcp-test-interactive-server-commands ()
  "Test that server commands are properly interactive."
  (should (commandp 'mcp-server-start))
  (should (commandp 'mcp-server-start-unix))
  (should (commandp 'mcp-server-start-tcp))
  (should (commandp 'mcp-server-stop))
  (should (commandp 'mcp-server-restart))
  (should (commandp 'mcp-server-status))
  (should (commandp 'mcp-server-toggle-debug)))

;;; Integration Tests

(ert-deftest mcp-test-server-full-lifecycle ()
  "Test complete server lifecycle."
  (let ((mcp-server-running nil))
    (with-mock
      (mock (mcp-server-transport-start * *) => t)
      (mock (mcp-server-tools-initialize))
      (mock (mcp-server-transport-stop *))
      
      (mcp-server-start)
      (should mcp-server-running)
      
      (let ((status (mcp-server-status)))
        (should (alist-get 'running status)))
      
      (mcp-server-stop)
      (should-not mcp-server-running))))

(ert-deftest mcp-test-server-configuration-persistence ()
  "Test that configuration changes persist across restarts."
  (let ((mcp-server-running nil)
        (mcp-server-debug nil))
    (with-mock
      (mock (mcp-server-transport-start * *) => t)
      (mock (mcp-server-tools-initialize))
      (mock (mcp-server-transport-stop *))
      
      (mcp-server-enable-debug)
      (mcp-server-start)
      (should mcp-server-debug)
      
      (mcp-server-restart)
      (should mcp-server-debug))))

(provide 'test-mcp-user-functions)
;;; test-mcp-user-functions.el ends here