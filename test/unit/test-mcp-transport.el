;;; test-mcp-transport.el --- Tests for MCP Transport Layer -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for transport layer including Unix socket handling,
;; multi-client connections, and message buffering.

;;; Code:

(require 'ert)
(require 'test-helpers)

;;; Transport Registry Tests

(ert-deftest mcp-test-transport-register ()
  "Test transport registration."
  (let ((mcp-server-transport--registry (make-hash-table :test 'equal)))
    (mcp-server-transport-register "test-transport"
                                   (lambda (&rest args) "test-implementation"))
    (should (mcp-server-transport-exists-p "test-transport"))
    (should-not (mcp-server-transport-exists-p "nonexistent-transport"))))

(ert-deftest mcp-test-transport-get ()
  "Test transport retrieval."
  (let ((mcp-server-transport--registry (make-hash-table :test 'equal)))
    (let ((impl (lambda (&rest args) "test-implementation")))
      (mcp-server-transport-register "test-transport" impl)
      (should (equal (mcp-server-transport-get "test-transport") impl))
      (should-not (mcp-server-transport-get "nonexistent")))))

(ert-deftest mcp-test-transport-list ()
  "Test transport listing."
  (let ((mcp-server-transport--registry (make-hash-table :test 'equal)))
    (mcp-server-transport-register "transport1" (lambda (&rest args) "impl1"))
    (mcp-server-transport-register "transport2" (lambda (&rest args) "impl2"))
    (let ((transports (mcp-server-transport-list)))
      (should (member "transport1" transports))
      (should (member "transport2" transports))
      (should (= (length transports) 2)))))

;;; Unix Socket Transport Tests

(ert-deftest mcp-test-unix-socket-naming-default ()
  "Test default Unix socket naming."
  (let ((mcp-server-socket-name nil)
        (mcp-server-socket-directory "/tmp/test/"))
    (should (string-suffix-p "emacs-mcp-server.sock"
                             (mcp-server-transport-unix--generate-socket-path)))))

(ert-deftest mcp-test-unix-socket-naming-custom ()
  "Test custom Unix socket naming."
  (let ((mcp-server-socket-name "custom-instance")
        (mcp-server-socket-directory "/tmp/test/"))
    (should (string-suffix-p "emacs-mcp-server-custom-instance.sock"
                             (mcp-server-transport-unix--generate-socket-path)))))

(ert-deftest mcp-test-unix-socket-naming-user ()
  "Test user-based Unix socket naming."
  (let ((mcp-server-socket-name 'user)
        (mcp-server-socket-directory "/tmp/test/"))
    (let ((socket-path (mcp-server-transport-unix--generate-socket-path)))
      (should (string-match-p "emacs-mcp-server-.*\\.sock$" socket-path))
      (should (string-match-p (user-login-name) socket-path)))))

(ert-deftest mcp-test-unix-socket-naming-session ()
  "Test session-based Unix socket naming."
  (let ((mcp-server-socket-name 'session)
        (mcp-server-socket-directory "/tmp/test/"))
    (let ((socket-path (mcp-server-transport-unix--generate-socket-path)))
      (should (string-match-p "emacs-mcp-server-.*-.*\\.sock$" socket-path))
      (should (string-match-p (user-login-name) socket-path))
      (should (string-match-p (number-to-string (emacs-pid)) socket-path)))))

(ert-deftest mcp-test-unix-socket-naming-function ()
  "Test function-based Unix socket naming."
  (let ((mcp-server-socket-name (lambda () "dynamic-name"))
        (mcp-server-socket-directory "/tmp/test/"))
    (should (string-suffix-p "emacs-mcp-server-dynamic-name.sock"
                             (mcp-server-transport-unix--generate-socket-path)))))

;;; Socket Conflict Resolution Tests

(ert-deftest mcp-test-unix-socket-conflict-warn ()
  "Test socket conflict resolution with warn strategy."
  (mcp-test-with-temp-socket
   (let ((mcp-server-socket-conflict-resolution 'warn))
     (with-temp-file mcp-test-socket-path
       (insert ""))
     (should (file-exists-p mcp-test-socket-path))
     (let ((result (mcp-server-transport-unix--handle-socket-conflict mcp-test-socket-path)))
       (should result)
       (should (string-match-p "emacs-mcp-server.*\\.sock$" result))
       (should-not (equal result mcp-test-socket-path))))))

(ert-deftest mcp-test-unix-socket-conflict-error ()
  "Test socket conflict resolution with error strategy."
  (mcp-test-with-temp-socket
   (let ((mcp-server-socket-conflict-resolution 'error))
     (with-temp-file mcp-test-socket-path
       (insert ""))
     (should-error (mcp-server-transport-unix--handle-socket-conflict mcp-test-socket-path)))))

(ert-deftest mcp-test-unix-socket-conflict-force ()
  "Test socket conflict resolution with force strategy."
  (mcp-test-with-temp-socket
   (let ((mcp-server-socket-conflict-resolution 'force))
     (with-temp-file mcp-test-socket-path
       (insert ""))
     (should (file-exists-p mcp-test-socket-path))
     (let ((result (mcp-server-transport-unix--handle-socket-conflict mcp-test-socket-path)))
       (should (equal result mcp-test-socket-path))))))

(ert-deftest mcp-test-unix-socket-conflict-auto ()
  "Test socket conflict resolution with auto strategy."
  (mcp-test-with-temp-socket
   (let ((mcp-server-socket-conflict-resolution 'auto))
     (with-temp-file mcp-test-socket-path
       (insert ""))
     (let ((result (mcp-server-transport-unix--handle-socket-conflict mcp-test-socket-path)))
       (should result)
       (should (string-match-p "emacs-mcp-server.*\\.sock$" result))
       (should-not (equal result mcp-test-socket-path))))))

;;; Message Processing Tests

(ert-deftest mcp-test-transport-message-parsing ()
  "Test transport message parsing."
  (let* ((test-message '((jsonrpc . "2.0") (method . "test") (id . "123")))
         (json-string (json-encode test-message))
         (parsed (mcp-server-transport-unix--parse-message json-string)))
    (should (equal (alist-get 'jsonrpc parsed) "2.0"))
    (should (equal (alist-get 'method parsed) "test"))
    (should (equal (alist-get 'id parsed) "123"))))

(ert-deftest mcp-test-transport-message-parsing-invalid ()
  "Test transport message parsing with invalid JSON."
  (should-error (mcp-server-transport-unix--parse-message "invalid json")))

(ert-deftest mcp-test-transport-message-fragmentation ()
  "Test handling of fragmented messages."
  (let ((mcp-server-transport-unix--message-buffers (make-hash-table :test 'equal)))
    (let ((fragment1 "{\"jsonrpc\":\"2.0\",")
          (fragment2 "\"method\":\"test\",\"id\":\"123\"}"))
      (mcp-server-transport-unix--buffer-message "client1" fragment1)
      (should (gethash "client1" mcp-server-transport-unix--message-buffers))
      (let ((complete-msg (mcp-server-transport-unix--buffer-message "client1" fragment2)))
        (should complete-msg)
        (let ((parsed (json-read-from-string complete-msg)))
          (should (equal (alist-get 'method parsed) "test")))))))

;;; Client Connection Management Tests

(ert-deftest mcp-test-transport-client-registration ()
  "Test client registration and tracking."
  (let ((mcp-server-transport-unix--clients (make-hash-table :test 'equal)))
    (mcp-server-transport-unix--register-client "client1" nil)
    (should (gethash "client1" mcp-server-transport-unix--clients))
    (should (= (hash-table-count mcp-server-transport-unix--clients) 1))))

(ert-deftest mcp-test-transport-client-deregistration ()
  "Test client deregistration and cleanup."
  (let ((mcp-server-transport-unix--clients (make-hash-table :test 'equal))
        (mcp-server-transport-unix--message-buffers (make-hash-table :test 'equal)))
    (mcp-server-transport-unix--register-client "client1" nil)
    (puthash "client1" "partial message" mcp-server-transport-unix--message-buffers)
    (mcp-server-transport-unix--deregister-client "client1")
    (should-not (gethash "client1" mcp-server-transport-unix--clients))
    (should-not (gethash "client1" mcp-server-transport-unix--message-buffers))))

(ert-deftest mcp-test-transport-list-clients ()
  "Test listing connected clients."
  (let ((mcp-server-transport-unix--clients (make-hash-table :test 'equal)))
    (mcp-server-transport-unix--register-client "client1" nil)
    (mcp-server-transport-unix--register-client "client2" nil)
    (let ((clients (mcp-server-transport-unix-list-clients)))
      (should (member "client1" clients))
      (should (member "client2" clients))
      (should (= (length clients) 2)))))

;;; Process Management Tests

(ert-deftest mcp-test-transport-process-cleanup ()
  "Test process cleanup on server stop."
  (let ((mcp-server-transport-unix--server-process 'mock-process)
        (mcp-server-transport-unix--clients (make-hash-table :test 'equal))
        (mcp-server-transport-unix--socket-path "/tmp/test.sock"))
    (puthash "client1" 'mock-client-process mcp-server-transport-unix--clients)
    (with-mock
      (mock (process-live-p 'mock-process) => t)
      (mock (delete-process 'mock-process))
      (mock (process-live-p 'mock-client-process) => t)
      (mock (delete-process 'mock-client-process))
      (mock (file-exists-p "/tmp/test.sock") => t)
      (mock (delete-file "/tmp/test.sock"))
      (mcp-server-transport-unix-stop)
      (should-not mcp-server-transport-unix--server-process)
      (should (= (hash-table-count mcp-server-transport-unix--clients) 0))
      (should-not mcp-server-transport-unix--socket-path))))

;;; Logging Tests

(ert-deftest mcp-test-transport-logging ()
  "Test transport logging functionality."
  (let ((mcp-server-debug t)
        (logged-messages '()))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) logged-messages))))
      (mcp-server-transport-unix--debug "Test debug message: %s" "value")
      (should (string-match-p "\\[MCP UNIX DEBUG\\].*Test debug message: value"
                              (car logged-messages))))))

(ert-deftest mcp-test-transport-no-logging-when-disabled ()
  "Test that transport logging is disabled when debug is off."
  (let ((mcp-server-debug nil)
        (logged-messages '()))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) logged-messages))))
      (mcp-server-transport-unix--debug "Test debug message")
      (should-not logged-messages))))

;;; Integration Tests

(ert-deftest mcp-test-transport-start-stop-cycle ()
  "Test complete transport start/stop cycle."
  (mcp-test-with-temp-dir
   (let ((mcp-server-socket-directory mcp-test-temp-dir)
         (mcp-server-socket-name "test-integration"))
     (let ((mock-handler (lambda (message client-id) 
                           (list "mock response" message client-id))))
       (with-mock
         (mock (make-network-process *) => 'mock-process)
         (let ((result (mcp-server-transport-unix--start mock-handler)))
           (should result)
           (should mcp-server-transport-unix--socket-path)))
       (with-mock
         (mock (process-live-p *) => t)
         (mock (delete-process *))
         (mock (file-exists-p *) => t)
         (mock (delete-file *))
         (mcp-server-transport-unix-stop)
         (should-not mcp-server-transport-unix--server-process))))))

(ert-deftest mcp-test-transport-concurrent-clients ()
  "Test handling of multiple concurrent clients."
  (let ((mcp-server-transport-unix--clients (make-hash-table :test 'equal))
        (mcp-server-transport-unix--message-buffers (make-hash-table :test 'equal)))
    (mcp-server-transport-unix--register-client "client1" 'process1)
    (mcp-server-transport-unix--register-client "client2" 'process2)
    (mcp-server-transport-unix--register-client "client3" 'process3)
    
    (mcp-server-transport-unix--buffer-message "client1" "{\"test\":\"msg1\"}")
    (mcp-server-transport-unix--buffer-message "client2" "{\"test\":\"msg2\"}")
    
    (should (= (hash-table-count mcp-server-transport-unix--clients) 3))
    (should (gethash "client1" mcp-server-transport-unix--message-buffers))
    (should (gethash "client2" mcp-server-transport-unix--message-buffers))
    (should-not (gethash "client3" mcp-server-transport-unix--message-buffers))))

(provide 'test-mcp-transport)
;;; test-mcp-transport.el ends here