;;; test-json-false.el --- Test JSON false serialization -*- lexical-binding: t; -*-

(require 'json)

(defun test-json-false ()
  "Test different ways of representing JSON false."
  (interactive)
  (message "=== Testing JSON false representations ===")
  
  ;; Test direct serialization
  (message "Direct :false -> %s" (json-serialize :false))
  
  ;; Test in hash table
  (let ((ht (make-hash-table :test 'equal)))
    (puthash "isError" :false ht)
    (message "Hash with :false -> %s" (json-serialize ht)))
  
  ;; Test our conversion function
  (let ((alist '((isError . :false))))
    (message "Original alist: %S" alist)
    (let ((converted (mcp-server-transport--alist-to-json alist)))
      (message "After conversion: %S" converted)
      (message "Final JSON: %s" (json-serialize converted))))
  
  ;; Test with actual response structure
  (let ((response '((jsonrpc . "2.0")
                    (id . 1)
                    (result . ((content . [((type . "text") (text . "result"))])
                               (isError . :false))))))
    (message "Test response: %S" response)
    (let ((converted (mcp-server-transport--alist-to-json response)))
      (message "Converted: %S" converted)
      (message "JSON: %s" (json-serialize converted)))))

(provide 'test-json-false)
;;; test-json-false.el ends here