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

(ert-deftest racer--read-rust-string ()
  (should
   (equal
    (racer--read-rust-string "\"foo \\n \\\" \\' \\; bar")
    "foo \n \" ' ; bar")))

(ert-deftest racer--help-buf ()
  (should
   (bufferp
    (racer--help-buf "foo bar."))))

(ert-deftest racer--propertize-all-inline-code ()
  (should
   (equal-including-properties
    (racer--propertize-all-inline-code "foo `bar` [`baz`] biz")
    #("foo bar baz biz" 4 7 (face font-lock-variable-name-face) 8 11 (face font-lock-variable-name-face)))))

(ert-deftest racer--propertize-docstring-code ()
  "Ensure we render code blocks with indents."
  (should
   (equal
    (racer--propertize-docstring "foo

```rust
func();
```

bar.

```text
1
2
```
")
    "foo

    func();

bar.

    1
    2
")))

(ert-deftest racer--propertize-docstring-newlines ()
  "Ensure we still handle links that have been split over two lines."
  (should
   (equal
    (racer--propertize-docstring "[foo\nbar](baz)")
    "foo\nbar")))

(ert-deftest racer--propertize-docstring-links ()
  "Ensure we discard footnote links."
  (should
   (equal
    (racer--propertize-docstring "foo [`str`] bar

[`str`]: ../../std/primitive.str.html

baz.")
    "foo str bar

baz.")))

(ert-deftest racer--propertize-docstring-urls ()
  "Ensure we render buttons for links with urls."
  (let ((result (racer--propertize-docstring "[foo](http://example.com)")))
    (should (equal result "foo"))
    (should (equal (get-text-property 0 'button result) '(t))))
  (should
   (equal-including-properties
    (racer--propertize-docstring "[foo](#bar)")
    "foo")))

(ert-deftest racer--propertize-docstring-heading ()
  "Ensure we render markdown headings correctly."
  (should
   (equal-including-properties
    (racer--propertize-docstring "# foo")
    #("foo" 0 3 (face racer-help-heading-face)))))

(ert-deftest racer--split-parts ()
  "Ensure we correctly parse racer CSV."
  (should
   (equal (racer--split-parts "foo;bar")
          '("foo" "bar")))
  (should
   (equal (racer--split-parts "foo;\"bar\"")
          '("foo" "bar")))
  (should
   (equal (racer--split-parts "foo\\;bar;baz")
          '("foo;bar" "baz"))))

(ert-deftest racer--describe-at-point-name ()
  "Ensure we extract the correct name in `racer--describe-at-point'."
  (cl-letf (((symbol-function 'racer--call)
             (lambda (&rest _)
               (list
                "PREFIX 36,37,n"
                "MATCH new;new();294;11;/home/user/src/rustc-1.10.0/src/libstd/../libcollections/vec.rs;Function;pub fn new() -> Vec<T>;\"Constructs a new, empty `Vec<T>`.\""
                "END"))))
    (should
     (equal (plist-get (racer--describe-at-point "new") :name)
            "new"))))

(ert-deftest racer--describe-at-point-nil-docstring ()
  "If there's no docstring, racer--describe-at-point should use nil."
  (cl-letf (((symbol-function 'racer--call)
             (lambda (&rest _)
               (list
                "PREFIX 36,37,n"
                "MATCH new;new();294;11;/home/user/src/rustc-1.10.0/src/libstd/../libcollections/vec.rs;Function;pub fn new() -> Vec<T>;\"\""
                "END"))))
    (should
     (null (plist-get (racer--describe-at-point "new") :docstring)))))

(ert-deftest racer--describe-at-point-shortest ()
  "If there are multiple matches, we want the shortest.

Since we've moved point to the end of symbol, the other functions just happen to have the same prefix."
  (cl-letf (((symbol-function 'racer--call)
             (lambda (&rest _)
               (list
                "PREFIX 36,37,n"
                "MATCH new_bar;new_bar();294;11;/home/user/src/rustc-1.10.0/src/libstd/../libcollections/vec.rs;Function;pub fn new() -> Vec<T>;\"\""
                "MATCH new;new();294;11;/home/user/src/rustc-1.10.0/src/libstd/../libcollections/vec.rs;Function;pub fn new() -> Vec<T>;\"\""
                "MATCH new_foo;new_foo();294;11;/home/user/src/rustc-1.10.0/src/libstd/../libcollections/vec.rs;Function;pub fn new() -> Vec<T>;\"\""
                "END"))))
    (should
     (equal (plist-get (racer--describe-at-point "new") :name)
            "new"))))

(ert-deftest racer--syntax-highlight ()
  "Ensure we highlight code blocks and snippets correctly."
  ;; Highlighting types should always use the type face.
  (should
   (equal-including-properties
    (racer--syntax-highlight "Foo")
    #("Foo" 0 3 (face font-lock-type-face))))
  ;; Highlighting keywords.
  (should
   (equal-including-properties
    (racer--syntax-highlight "false")
    #("false" 0 5 (face font-lock-keyword-face))))
  ;; Simple variables should be highlighted, even when standalone.
  (should
   (equal-including-properties
    (racer--syntax-highlight "foo")
    #("foo" 0 3 (face font-lock-variable-name-face)))))

(ert-deftest racer-describe ()
  "Smoke test for `racer-describe'."
  (cl-letf (((symbol-function 'racer--call)
             (lambda (&rest _)
               (list
                "PREFIX 36,37,n"
                "MATCH foo;foo();294;11;/home/user/src/rustc-1.10.0/src/libstd/../libcollections/vec.rs;Function;pub fn new() -> Vec<T>;\"\""
                "END"))))
    (with-temp-buffer
      (rust-mode)
      (insert "foo();")
      (goto-char (point-min))
      (racer-describe))))

(ert-deftest racer-describe-uses-whole-symbol ()
  "Racer uses the symbol *before* point, so make sure we move point to
the end of the current symbol.

Otherwise, if the point is at the start of the symbol, we don't find anything."
  (let (point-during-call)
    (cl-letf (((symbol-function 'racer--call)
               (lambda (&rest _)
                 (setq point-during-call (point))
                 (list
                  "PREFIX 36,37,n"
                  "MATCH foo;foo();294;11;/home/user/src/rustc-1.10.0/src/libstd/../libcollections/vec.rs;Function;pub fn new() -> Vec<T>;\"\""
                  "END"))))
      (with-temp-buffer
        (rust-mode)
        (insert "foo();")
        (goto-char (point-min))
        ;; This should move point to the end of 'foo' before calling
        ;; racer--call.
        (racer-describe))
      (should (equal point-during-call 4)))))
