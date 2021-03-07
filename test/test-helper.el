;;; test-helper --- Test helper for racer

;;; Commentary:

;;; Code:

(require 'ert)
(require 'f)

(let ((racer-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path racer-dir))

(require 'undercover)
(undercover "racer.el" (:exclude "*-test.el"))

;;; test-helper.el ends here
