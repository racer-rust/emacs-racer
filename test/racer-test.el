(require 'racer)
(require 'ert)

(ert-deftest racer--file-and-parent ()
  (should
   (equal
    (racer--file-and-parent "/foo/bar/baz/q.txt")
    "baz/q.txt")))

(ert-deftest racer--goto-func-name ()
  (with-temp-buffer
    ;; Insert a function call.
    (insert "foo(bar, baz);")
    ;; Move to the start of the second argument.
    (goto-char (point-min))
    (while (not (looking-at "baz"))
      (forward-char 1))
    ;; We should be at the end of foo.
    (racer--goto-func-name)
    (should (equal (point) 4))))
