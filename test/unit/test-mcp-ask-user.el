;;; test-mcp-ask-user.el --- Tests for the ask-user MCP tool -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for tools/mcp-server-emacs-tools-ask-user.el.
;;
;; The transient UI cannot be exercised in batch mode (no display), so
;; mcp-server-emacs-tools-ask-user--show is mocked throughout.  The tests
;; therefore cover:
;;
;;   - Tool registration (name, function, schema)
;;   - Input validation (missing/wrong-type questions, missing/non-string
;;     title per entry, missing/empty/wrong-type options per entry,
;;     over-length inputs)
;;   - Handler returns mcp-deferred
;;   - Handler passes correct question structs to --show
;;   - Queue: second call while first is in-flight is enqueued, not
;;     dispatched immediately
;;   - Queue: completing the first call dispatches the second
;;   - Queue: C-g abort (nil callback) sends tool error and unblocks queue
;;   - Navigation: --navigate clamps correctly
;;   - Submit: sends JSON array of answers in question order
;;   - Submit: no-op when not all questions answered

;;; Code:

(require 'ert)
(require 'test-helpers)
(require 'mcp-server-tools)
(require 'mcp-server)

;; Load the tool (self-registers on require).
(let* ((this-file (or load-file-name buffer-file-name))
       (tools-dir (and this-file
                       (expand-file-name "../../tools"
                                         (file-name-directory this-file)))))
  (when tools-dir
    (add-to-list 'load-path tools-dir)))
(require 'mcp-server-emacs-tools-ask-user)

;;; --------------------------------------------------------------------------
;;; Helpers
;;; --------------------------------------------------------------------------

(defmacro mcp-test-ask-user-with-mock-show (&rest body)
  "Execute BODY with `mcp-server-emacs-tools-ask-user--show' mocked to a no-op.
The mock records calls in `mcp-test-ask-user--last-show-questions' and
captures the callback in `mcp-test-ask-user--captured-callback'."
  `(let (mcp-test-ask-user--last-show-questions
         mcp-test-ask-user--captured-callback)
     (mcp-test-with-mock
      ((mcp-server-emacs-tools-ask-user--show
        (lambda (questions callback)
          (setq mcp-test-ask-user--last-show-questions questions
                mcp-test-ask-user--captured-callback   callback))))
      ,@body)))

(defmacro mcp-test-ask-user-reset-queue ()
  "Reset queue state between tests."
  `(progn
     (setq mcp-server-emacs-tools-ask-user--queue    nil)
     (setq mcp-server-emacs-tools-ask-user--in-flight nil)))

;;; --------------------------------------------------------------------------
;;; Registration
;;; --------------------------------------------------------------------------

(ert-deftest mcp-test-ask-user-registered ()
  "ask-user tool is registered after require."
  (should (mcp-server-tools-exists-p "ask-user")))

(ert-deftest mcp-test-ask-user-tool-metadata ()
  "ask-user tool has correct name, non-nil function and input-schema."
  (let ((tool (mcp-server-tools-get "ask-user")))
    (should tool)
    (should (equal "ask-user" (mcp-server-tool-name tool)))
    (should (mcp-server-tool-function tool))
    (should (mcp-server-tool-input-schema tool))))

;;; --------------------------------------------------------------------------
;;; Input validation — top-level
;;; --------------------------------------------------------------------------

(ert-deftest mcp-test-ask-user-validation-missing-questions ()
  "Handler errors when questions key is absent."
  (mcp-test-with-mock-server
   (mcp-test-ask-user-with-mock-show
    (mcp-test-ask-user-reset-queue)
    (let ((mcp-server--current-client-id  "c")
          (mcp-server--current-request-id "r"))
      (should-error
       (mcp-server-emacs-tools-ask-user--handler '()))))))

(ert-deftest mcp-test-ask-user-validation-questions-not-array ()
  "Handler errors when questions is not an array."
  (mcp-test-with-mock-server
   (mcp-test-ask-user-with-mock-show
    (mcp-test-ask-user-reset-queue)
    (let ((mcp-server--current-client-id  "c")
          (mcp-server--current-request-id "r"))
      (should-error
       (mcp-server-emacs-tools-ask-user--handler
        '((questions . "not-an-array"))))))))

(ert-deftest mcp-test-ask-user-validation-questions-empty ()
  "Handler errors when questions array is empty."
  (mcp-test-with-mock-server
   (mcp-test-ask-user-with-mock-show
    (mcp-test-ask-user-reset-queue)
    (let ((mcp-server--current-client-id  "c")
          (mcp-server--current-request-id "r"))
      (should-error
       (mcp-server-emacs-tools-ask-user--handler
        '((questions . []))))))))

;;; --------------------------------------------------------------------------
;;; Input validation — per-question
;;; --------------------------------------------------------------------------

(ert-deftest mcp-test-ask-user-validation-missing-title ()
  "Handler errors when a question entry has no title."
  (mcp-test-with-mock-server
   (mcp-test-ask-user-with-mock-show
    (mcp-test-ask-user-reset-queue)
    (let ((mcp-server--current-client-id  "c")
          (mcp-server--current-request-id "r"))
      (should-error
       (mcp-server-emacs-tools-ask-user--handler
        '((questions . [((options . ["a" "b"]))]))))))))

(ert-deftest mcp-test-ask-user-validation-non-string-title ()
  "Handler errors when a question title is not a string."
  (mcp-test-with-mock-server
   (mcp-test-ask-user-with-mock-show
    (mcp-test-ask-user-reset-queue)
    (let ((mcp-server--current-client-id  "c")
          (mcp-server--current-request-id "r"))
      (should-error
       (mcp-server-emacs-tools-ask-user--handler
        '((questions . [((title . 42) (options . ["a"]))]))))))))

(ert-deftest mcp-test-ask-user-validation-missing-options ()
  "Handler errors when a question entry has no options."
  (mcp-test-with-mock-server
   (mcp-test-ask-user-with-mock-show
    (mcp-test-ask-user-reset-queue)
    (let ((mcp-server--current-client-id  "c")
          (mcp-server--current-request-id "r"))
      (should-error
       (mcp-server-emacs-tools-ask-user--handler
        '((questions . [((title . "Q?"))]))))))))

(ert-deftest mcp-test-ask-user-validation-empty-options ()
  "Handler errors when a question options array is empty."
  (mcp-test-with-mock-server
   (mcp-test-ask-user-with-mock-show
    (mcp-test-ask-user-reset-queue)
    (let ((mcp-server--current-client-id  "c")
          (mcp-server--current-request-id "r"))
      (should-error
       (mcp-server-emacs-tools-ask-user--handler
        '((questions . [((title . "Q?") (options . []))]))))))))

(ert-deftest mcp-test-ask-user-validation-non-string-option ()
  "Handler errors when an option item is not a string."
  (mcp-test-with-mock-server
   (mcp-test-ask-user-with-mock-show
    (mcp-test-ask-user-reset-queue)
    (let ((mcp-server--current-client-id  "c")
          (mcp-server--current-request-id "r"))
      (should-error
       (mcp-server-emacs-tools-ask-user--handler
        '((questions . [((title . "Q?") (options . ["ok" 99]))]))))))))

(ert-deftest mcp-test-ask-user-validation-title-too-long ()
  "Handler errors when a question title exceeds max-input-length."
  (mcp-test-with-mock-server
   (mcp-test-ask-user-with-mock-show
    (mcp-test-ask-user-reset-queue)
    (let ((mcp-server-emacs-tools-ask-user-max-input-length 5)
          (mcp-server--current-client-id  "c")
          (mcp-server--current-request-id "r"))
      (should-error
       (mcp-server-emacs-tools-ask-user--handler
        '((questions . [((title . "too long title") (options . ["a"]))]))))))))

(ert-deftest mcp-test-ask-user-validation-option-too-long ()
  "Handler errors when an option exceeds max-input-length."
  (mcp-test-with-mock-server
   (mcp-test-ask-user-with-mock-show
    (mcp-test-ask-user-reset-queue)
    (let ((mcp-server-emacs-tools-ask-user-max-input-length 5)
          (mcp-server--current-client-id  "c")
          (mcp-server--current-request-id "r"))
      (should-error
       (mcp-server-emacs-tools-ask-user--handler
        '((questions . [((title . "Q?") (options . ["ok" "too long option"]))]))))))))

;;; --------------------------------------------------------------------------
;;; Handler return value and UI
;;; --------------------------------------------------------------------------

(ert-deftest mcp-test-ask-user-handler-returns-mcp-deferred ()
  "Handler returns mcp-deferred for a valid single-question call."
  (mcp-test-with-mock-server
   (mcp-test-ask-user-with-mock-show
    (mcp-test-ask-user-reset-queue)
    (let ((mcp-server--current-client-id  "c")
          (mcp-server--current-request-id "r"))
      (should (eq 'mcp-deferred
                  (mcp-server-emacs-tools-ask-user--handler
                   '((questions . [((title . "Pick one")
                                    (options . ["A" "B"]))])))))))))

(ert-deftest mcp-test-ask-user-handler-multi-question-returns-mcp-deferred ()
  "Handler returns mcp-deferred for a multi-question call."
  (mcp-test-with-mock-server
   (mcp-test-ask-user-with-mock-show
    (mcp-test-ask-user-reset-queue)
    (let ((mcp-server--current-client-id  "c")
          (mcp-server--current-request-id "r"))
      (should (eq 'mcp-deferred
                  (mcp-server-emacs-tools-ask-user--handler
                   '((questions . [((title . "Q1") (options . ["A" "B"]))
                                   ((title . "Q2") (options . ["X" "Y"]))])))))))))

(ert-deftest mcp-test-ask-user-handler-shows-correct-questions ()
  "Handler passes structs with correct titles and choices to --show."
  (mcp-test-with-mock-server
   (mcp-test-ask-user-with-mock-show
    (mcp-test-ask-user-reset-queue)
    (let ((mcp-server--current-client-id  "c")
          (mcp-server--current-request-id "r"))
      (mcp-server-emacs-tools-ask-user--handler
       '((questions . [((title . "Colour?") (options . ["Red" "Blue"]))
                       ((title . "Size?")   (options . ["S" "M" "L"]))])))
      (let ((qs mcp-test-ask-user--last-show-questions))
        (should (= 2 (length qs)))
        (should (equal "Colour?"
                       (mcp-server-emacs-tools-ask-user--question-title (nth 0 qs))))
        (should (equal '("Red" "Blue")
                       (mcp-server-emacs-tools-ask-user--question-choices (nth 0 qs))))
        (should (equal "Size?"
                       (mcp-server-emacs-tools-ask-user--question-title (nth 1 qs))))
        (should (equal '("S" "M" "L")
                       (mcp-server-emacs-tools-ask-user--question-choices (nth 1 qs)))))))))

;;; --------------------------------------------------------------------------
;;; Navigation
;;; --------------------------------------------------------------------------

(ert-deftest mcp-test-ask-user-navigate-increments-index ()
  "Navigation forward increments current index."
  (setq mcp-server-emacs-tools-ask-user--questions
        (list (make-mcp-server-emacs-tools-ask-user--question
               :id "1" :title "Q1" :choices '("A") :selected-choice nil)
              (make-mcp-server-emacs-tools-ask-user--question
               :id "2" :title "Q2" :choices '("B") :selected-choice nil)))
  (setq mcp-server-emacs-tools-ask-user--current-index 0)
  (mcp-test-with-mock
   ((transient-setup (lambda (_) nil)))
   (mcp-server-emacs-tools-ask-user--navigate 1)
   (should (= 1 mcp-server-emacs-tools-ask-user--current-index))))

(ert-deftest mcp-test-ask-user-navigate-clamps-at-end ()
  "Navigation forward clamps at last question."
  (setq mcp-server-emacs-tools-ask-user--questions
        (list (make-mcp-server-emacs-tools-ask-user--question
               :id "1" :title "Q1" :choices '("A") :selected-choice nil)
              (make-mcp-server-emacs-tools-ask-user--question
               :id "2" :title "Q2" :choices '("B") :selected-choice nil)))
  (setq mcp-server-emacs-tools-ask-user--current-index 1)
  (mcp-test-with-mock
   ((transient-setup (lambda (_) nil)))
   (mcp-server-emacs-tools-ask-user--navigate 1)
   (should (= 1 mcp-server-emacs-tools-ask-user--current-index))))

(ert-deftest mcp-test-ask-user-navigate-clamps-at-start ()
  "Navigation backward clamps at first question."
  (setq mcp-server-emacs-tools-ask-user--questions
        (list (make-mcp-server-emacs-tools-ask-user--question
               :id "1" :title "Q1" :choices '("A") :selected-choice nil)))
  (setq mcp-server-emacs-tools-ask-user--current-index 0)
  (mcp-test-with-mock
   ((transient-setup (lambda (_) nil)))
   (mcp-server-emacs-tools-ask-user--navigate -1)
   (should (= 0 mcp-server-emacs-tools-ask-user--current-index))))

;;; --------------------------------------------------------------------------
;;; Submit behaviour
;;; --------------------------------------------------------------------------

(ert-deftest mcp-test-ask-user-submit-noop-when-unanswered ()
  "Submit is a no-op when not all questions are answered."
  (let (callback-called)
    (setq mcp-server-emacs-tools-ask-user--questions
          (list (make-mcp-server-emacs-tools-ask-user--question
                 :id "1" :title "Q1" :choices '("A" "B") :selected-choice "A")
                (make-mcp-server-emacs-tools-ask-user--question
                 :id "2" :title "Q2" :choices '("X" "Y") :selected-choice nil)))
    (setq mcp-server-emacs-tools-ask-user--current-index 0
          mcp-server-emacs-tools-ask-user--callback
          (lambda (_) (setq callback-called t)))
    (mcp-test-with-mock
     ((transient-quit-all (lambda () nil)))
     (mcp-server-emacs-tools-ask-user--submit)
     (should-not callback-called))))

(ert-deftest mcp-test-ask-user-submit-sends-answers-array ()
  "Submit calls callback with vector of answers in question order."
  (let (received-answers)
    (setq mcp-server-emacs-tools-ask-user--questions
          (list (make-mcp-server-emacs-tools-ask-user--question
                 :id "1" :title "Q1" :choices '("A" "B") :selected-choice "A")
                (make-mcp-server-emacs-tools-ask-user--question
                 :id "2" :title "Q2" :choices '("X" "Y") :selected-choice "Y")))
    (setq mcp-server-emacs-tools-ask-user--current-index 0
          mcp-server-emacs-tools-ask-user--callback
          (lambda (answers) (setq received-answers answers)))
    (mcp-test-with-mock
     ((transient-quit-all (lambda () nil)))
     (mcp-server-emacs-tools-ask-user--submit)
     (should (equal ["A" "Y"] received-answers)))))

;;; --------------------------------------------------------------------------
;;; Queue behaviour
;;; --------------------------------------------------------------------------

(ert-deftest mcp-test-ask-user-queue-second-call-enqueued ()
  "A second call while the first is in-flight is enqueued, not shown."
  (mcp-test-with-mock-server
   (mcp-test-ask-user-with-mock-show
    (mcp-test-ask-user-reset-queue)
    (let ((mcp-server--current-client-id  "c1")
          (mcp-server--current-request-id "r1"))
      (mcp-server-emacs-tools-ask-user--handler
       '((questions . [((title . "Q1") (options . ["A"]))]))))
    (should mcp-server-emacs-tools-ask-user--in-flight)
    (let ((mcp-server--current-client-id  "c2")
          (mcp-server--current-request-id "r2"))
      (mcp-server-emacs-tools-ask-user--handler
       '((questions . [((title . "Q2") (options . ["B"]))])))
      ;; show must NOT have been called a second time — first question still shown.
      (should (equal "Q1"
                     (mcp-server-emacs-tools-ask-user--question-title
                      (car mcp-test-ask-user--last-show-questions))))
      (should (= 1 (length mcp-server-emacs-tools-ask-user--queue)))))))

(ert-deftest mcp-test-ask-user-queue-abort-sends-error-and-unblocks ()
  "C-g abort (callback called with nil) sends a tool error and unblocks the queue."
  (let (sent-responses)
    (mcp-test-with-mock
     ((mcp-server-transport-send-raw
       (lambda (_t _c json-str) (push json-str sent-responses))))
     (mcp-test-ask-user-with-mock-show
      (mcp-test-ask-user-reset-queue)
      ;; First call.
      (let ((mcp-server--current-client-id  "c1")
            (mcp-server--current-request-id "r1"))
        (mcp-server-emacs-tools-ask-user--handler
         '((questions . [((title . "Q1") (options . ["Yes" "No"]))]))))
      ;; Second call (enqueued while first is in-flight).
      (let ((mcp-server--current-client-id  "c2")
            (mcp-server--current-request-id "r2"))
        (mcp-server-emacs-tools-ask-user--handler
         '((questions . [((title . "Q2") (options . ["X" "Y"]))]))))
      ;; Simulate C-g: abort hook calls callback with nil.
      (funcall mcp-test-ask-user--captured-callback nil)
      ;; Q2 should now be shown (dispatch-next re-sets in-flight for it).
      (should (equal "Q2"
                     (mcp-server-emacs-tools-ask-user--question-title
                      (car mcp-test-ask-user--last-show-questions))))
      ;; One error response should have been sent for r1.
      (should (= 1 (length sent-responses)))
      (let* ((parsed  (json-parse-string (car sent-responses)
                                         :object-type 'alist
                                         :array-type  'array))
             (result  (alist-get 'result parsed)))
        (should (equal "r1" (alist-get 'id parsed)))
        ;; Tool errors are sent as result with isError:true (MCP protocol).
        (should (eq t (alist-get 'isError result))))))))

(ert-deftest mcp-test-ask-user-queue-dispatches-next-on-completion ()
  "Completing the first call dispatches the queued second call."
  (let (sent-responses)
    (mcp-test-with-mock
     ((mcp-server-transport-send-raw
       (lambda (_t _c json-str) (push json-str sent-responses))))
     (mcp-test-ask-user-with-mock-show
      (mcp-test-ask-user-reset-queue)
      ;; First call.
      (let ((mcp-server--current-client-id  "c1")
            (mcp-server--current-request-id "r1"))
        (mcp-server-emacs-tools-ask-user--handler
         '((questions . [((title . "Q1") (options . ["Yes" "No"]))]))))
      ;; Second call (enqueued).
      (let ((mcp-server--current-client-id  "c2")
            (mcp-server--current-request-id "r2"))
        (mcp-server-emacs-tools-ask-user--handler
         '((questions . [((title . "Q2") (options . ["X" "Y"]))]))))
      ;; Simulate user answering Q1 via the captured callback.
      (funcall mcp-test-ask-user--captured-callback ["Yes"])
      ;; Q2 should now be shown.
      (should (equal "Q2"
                     (mcp-server-emacs-tools-ask-user--question-title
                      (car mcp-test-ask-user--last-show-questions))))
      ;; One response should have been sent (for Q1).
      (should (= 1 (length sent-responses)))
      (let ((parsed (json-parse-string (car sent-responses)
                                       :object-type 'alist
                                       :array-type  'array)))
        (should (equal "r1" (alist-get 'id parsed))))))))

(provide 'test-mcp-ask-user)
;;; test-mcp-ask-user.el ends here
