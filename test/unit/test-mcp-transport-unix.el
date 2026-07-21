;;; test-mcp-transport-unix.el --- Tests for unix transport socket lifecycle -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression tests for Unix domain socket lifecycle management:
;; stale-socket cleanup, active-socket preservation, stop idempotency,
;; and kill-emacs-hook registration.

;;; Code:

(require 'ert)
(require 'test-helpers)
(require 'mcp-server)
(require 'mcp-server-transport-unix)

(ert-deftest mcp-test-transport-cleanup-stale-socket ()
  "Cleanup removes a numbered socket file that is stale."
  (mcp-test-with-temp-dir
    (let ((mcp-server-transport-unix--socket-path
           (expand-file-name "emacs-mcp-server-test.sock" mcp-test-temp-dir))
          (stale-sock (expand-file-name "emacs-mcp-server-1.sock" mcp-test-temp-dir)))
      (with-temp-file stale-sock (insert "stale"))
      (mcp-server-transport-unix--cleanup-stale-sockets)
      (should-not (file-exists-p stale-sock)))))

(ert-deftest mcp-test-transport-preserves-active-socket ()
  "Cleanup preserves a numbered socket that is a live Unix socket."
  (mcp-test-with-temp-dir
    (let* ((mcp-server-transport-unix--socket-path
            (expand-file-name "emacs-mcp-server-test.sock" mcp-test-temp-dir))
           (live-sock (expand-file-name "emacs-mcp-server-2.sock" mcp-test-temp-dir))
           (server-proc (make-network-process
                         :name "test-live-sock"
                         :family 'local
                         :service live-sock
                         :server t
                         :noquery t)))
      (unwind-protect
          (progn
            (mcp-server-transport-unix--cleanup-stale-sockets)
            (should (file-exists-p live-sock)))
        (when (processp server-proc)
          (delete-process server-proc))
        (when (file-exists-p live-sock)
          (delete-file live-sock))))))

(ert-deftest mcp-test-transport-stop-idempotent ()
  "Calling --stop multiple times does not error."
  (mcp-test-with-temp-dir
    (let ((socket-path (expand-file-name "mcp-test.sock" mcp-test-temp-dir)))
      (unwind-protect
          (progn
            (mcp-server-transport-unix--start (lambda (&rest _) t) socket-path)
            (mcp-server-transport-unix--stop)
            (mcp-server-transport-unix--stop)
            (should-not mcp-server-transport-unix--running))
        (when mcp-server-transport-unix--server-process
          (delete-process mcp-server-transport-unix--server-process))
        (setq mcp-server-transport-unix--running nil
              mcp-server-transport-unix--socket-path nil
              mcp-server-transport-unix--message-handler nil)
        (remove-hook 'kill-emacs-hook 'mcp-server-transport-unix--stop)))))

(ert-deftest mcp-test-transport-kill-hook-registration ()
  "Starting the transport adds --stop to kill-emacs-hook."
  (mcp-test-with-temp-dir
    (let ((socket-path (expand-file-name "mcp-test.sock" mcp-test-temp-dir)))
      (unwind-protect
          (progn
            (mcp-server-transport-unix--start (lambda (&rest _) t) socket-path)
            (should (member 'mcp-server-transport-unix--stop kill-emacs-hook)))
        (when mcp-server-transport-unix--server-process
          (delete-process mcp-server-transport-unix--server-process))
        (setq mcp-server-transport-unix--running nil
              mcp-server-transport-unix--socket-path nil
              mcp-server-transport-unix--message-handler nil)
        (remove-hook 'kill-emacs-hook 'mcp-server-transport-unix--stop)))))

(provide 'test-mcp-transport-unix)
;;; test-mcp-transport-unix.el ends here
