;;; test-mcp-org-helpers.el --- Tests for org test helpers -*- lexical-binding: t; -*-
(require 'ert)
(require 'test-helpers)

(ert-deftest mcp-test-org-helpers-with-fixture-copy ()
  "mcp-test-with-org-fixture copies the fixture to a temp location."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (should (file-exists-p path))
    (should (string-match-p "^/.*sample-notes\\.org$" path))
    (with-temp-buffer
      (insert-file-contents path)
      (should (string-match-p "Project Alpha" (buffer-string))))))

(ert-deftest mcp-test-org-helpers-with-fixture-cleanup ()
  "mcp-test-with-org-fixture cleans up the temp path."
  (let (captured-path)
    (mcp-test-with-org-fixture "sample-notes.org" path
      (setq captured-path path)
      (should (file-exists-p captured-path)))
    (should-not (file-exists-p captured-path))))

(provide 'test-mcp-org-helpers)
;;; test-mcp-org-helpers.el ends here
