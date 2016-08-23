;;; test-helper --- Test helper for racer

;;; Code:

(require 'undercover)
(undercover "racer.el"
	    (:exclude "*-test.el")
	    (:report-file "/tmp/undercover-report.json"))

(provide 'test-helper)
;;; test-helper.el ends here
