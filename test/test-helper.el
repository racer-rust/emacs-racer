;;; test-helper --- Test helper for racer

;;; Commentary:

;;; Code:

(require 'undercover)
(undercover "racer.el"
            (:exclude "*-test.el")
            (:report-file "/tmp/undercover-report.json"))

;;; test-helper.el ends here
