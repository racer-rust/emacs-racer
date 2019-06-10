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

(ert-deftest racer--read-rust-string-backslash-n ()
  "Regression test for a literal backslash followed by an n, not
a newline."
  (should
   (equal
    (racer--read-rust-string "\\\\n")
    "\\n")))

(ert-deftest racer--help-buf ()
  (should
   (bufferp
    (racer--help-buf "foo bar."))))

(ert-deftest racer--propertize-all-inline-code ()
  (should
   (equal-including-properties
    (racer--propertize-all-inline-code "foo `bar` [`baz`] biz")
    #("foo bar baz biz" 4 7 (face font-lock-variable-name-face) 8 11 (face font-lock-variable-name-face)))))

(ert-deftest racer--propertize-all-inline-code-with-escaped-newlines ()
  "Regression test for an escaped newline at the end of inline code."
  (should
   (equal
    (racer--propertize-all-inline-code "foo `\\n`")
    "foo \\n")))

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
    2")))

(defun racer--remove-properties (text)
  "Remove all the properties on TEXT.
Tests that use `equal' ignore properties, but
this makes the ert failure descriptions clearer."
  (with-temp-buffer
    (insert text)
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest racer--propertize-docstring-code-annotations ()
  "Ignore '# foo' lines in code sections in docstrings."
  (should
   (equal
    (racer--remove-properties
     (racer--propertize-docstring "```
# #[allow(dead_code)]
#[derive(Debug)]
struct Foo {}
```"))
    "    #[derive(Debug)]
    struct Foo {}")))

(ert-deftest racer--propertize-docstring-code-newlines ()
  "Ensure we always have a blank line before a code block."
  
  (should
   (equal
    (racer--remove-properties
     (racer--propertize-docstring "```
bar1();
```
foo
```
bar2();
```"))
    "    bar1();

foo

    bar2();")))

(ert-deftest racer--propertize-docstring-newlines ()
  "Ensure we still handle links that have been split over two lines."
  (should
   (equal
    (racer--propertize-docstring "[foo\nbar](baz)")
    "foo\nbar")))

(ert-deftest racer--propertize-docstring-link-after-attribute ()
  "We should not confuse attributes with links."
  (should
   (equal
    (racer--remove-properties
     (racer--propertize-docstring "Result is annotated with the #[must_use] attribute,
by the [`Write`](../../std/io/trait.Write.html) trait"))
    "Result is annotated with the #[must_use] attribute,
by the Write trait")))

(ert-deftest racer--propertize-docstring-footnotes ()
  "Ensure we discard footnote links."
  (should
   (equal
    (racer--remove-properties
     (racer--propertize-docstring "foo [`str`] bar

\[`str`]: ../../std/primitive.str.html

baz."))
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
               (s-join
                "\n"
                (list
                 "PREFIX 36,37,n"
                 "MATCH new;new();294;11;/home/user/src/rustc-1.10.0/src/libstd/../libcollections/vec.rs;Function;pub fn new() -> Vec<T>;\"Constructs a new, empty `Vec<T>`.\""
                 "END")))))
    (dolist (desc (racer--describe-at-point "new"))
      (should
       (equal (plist-get desc :name) "new")))))

(ert-deftest racer--describe-at-point-nil-docstring ()
  "If there's no docstring, racer--describe-at-point should use nil."
  (cl-letf (((symbol-function 'racer--call)
             (lambda (&rest _)
               (s-join
                "\n"
                (list
                 "PREFIX 36,37,n"
                 "MATCH new;new();294;11;/home/user/src/rustc-1.10.0/src/libstd/../libcollections/vec.rs;Function;pub fn new() -> Vec<T>;\"\""
                 "END")))))
    (dolist (desc (racer--describe-at-point "new"))
      (should
       (null (plist-get desc :docstring))))))

(ert-deftest racer--describe-at-point-shortest ()
  "If there are multiple matches, we want the shortest.

Since we've moved point to the end of symbol, the other functions just happen to have the same prefix."
  (cl-letf (((symbol-function 'racer--call)
             (lambda (&rest _)
               (s-join
                "\n"
                (list
                 "PREFIX 36,37,n"
                 "MATCH new_bar;new_bar();294;11;/home/user/src/rustc-1.10.0/src/libstd/../libcollections/vec.rs;Function;pub fn new() -> Vec<T>;\"\""
                 "MATCH new;new();294;11;/home/user/src/rustc-1.10.0/src/libstd/../libcollections/vec.rs;Function;pub fn new() -> Vec<T>;\"\""
                 "MATCH new_foo;new_foo();294;11;/home/user/src/rustc-1.10.0/src/libstd/../libcollections/vec.rs;Function;pub fn new() -> Vec<T>;\"\""
                 "END")))))
    (dolist (desc (racer--describe-at-point "new"))
      (should
       (equal (plist-get desc :name) "new")))))

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
               (s-join
                "\n"
                (list
                 "PREFIX 36,37,n"
                 "MATCH foo;foo();294;11;/home/user/src/rustc-1.10.0/src/libstd/../libcollections/vec.rs;Function;pub fn new() -> Vec<T>;\"\""
                 "END")))))
    (with-temp-buffer
      (rust-mode)
      (insert "foo();")
      (goto-char (point-min))
      (racer-describe))))

(ert-deftest racer-describe-test-description ()
  "Ensure we write the correct text summary in the first line
of the racer describe buffer."
  (cl-letf (((symbol-function 'racer--call)
             (lambda (&rest _)
               "PREFIX 8,10,Ok\nMATCH Ok;Ok;253;4;/home/user/src/rustc-1.10.0/src/libstd/../libcore/result.rs;EnumVariant;Ok(#[stable(feature = \"rust1\", since = \"1.0.0\")] T),;\"`Result` is a type that represents either success (`Ok`) or failure (`Err`).\n\nSee the [`std::result`](index.html) module documentation for details.\nEND\n")))
    (with-temp-buffer
      (rust-mode)
      (insert "Ok")
      (goto-char (point-min))
      (switch-to-buffer (racer--describe "Ok"))
      (let ((first-line (-first-item (s-lines (buffer-substring-no-properties
                                               (point-min) (point-max))))))
        (should
         (equal first-line
                "Ok is an enum variant defined in libcore/result.rs."))))))

(ert-deftest racer-describe-module-description ()
  "Ensure we write the correct text summary in the first line
of the racer describe buffer."
  (cl-letf (((symbol-function 'racer--call)
             (lambda (&rest _)
               "PREFIX 13,20,matches\nMATCH matches;matches;1;0;/home/user/.cargo/registry/src/github.com-1ecc6299db9ec823/matches-0.1.2/lib.rs;Module;/home/user/.cargo/registry/src/github.com-1ecc6299db9ec823/matches-0.1.2/lib.rs;\"\"\nEND\n")))
    (with-temp-buffer
      (rust-mode)
      (insert "extern crate matches;")
      (goto-char (1- (point-max)))
      (switch-to-buffer (racer--describe "matches"))
      (should
       (equal (racer--remove-properties (buffer-string))
              "matches is a module defined in matches-0.1.2/lib.rs.

Not documented.")))))

(ert-deftest racer-describe-uses-whole-symbol ()
  "Racer uses the symbol *before* point, so make sure we move point to
the end of the current symbol.

Otherwise, if the point is at the start of the symbol, we don't find anything."
  (let (point-during-call)
    (cl-letf (((symbol-function 'racer--call)
               (lambda (&rest _)
                 (setq point-during-call (point))
                 (s-join
                  "\n"
                  (list
                   "PREFIX 36,37,n"
                   "MATCH foo;foo();294;11;/home/user/src/rustc-1.10.0/src/libstd/../libcollections/vec.rs;Function;pub fn new() -> Vec<T>;\"\""
                   "END")))))
      (with-temp-buffer
        (rust-mode)
        (insert "foo();")
        (goto-char (point-min))
        ;; This should move point to the end of 'foo' before calling
        ;; racer--call.
        (racer-describe))
      (should (equal point-during-call 4)))))

(ert-deftest racer-debug ()
  "Smoke test for `racer-debug'."
  (let ((racer--prev-state
         (list
          :program "racer"
          :args '("complete" "1" "2")
          :exit-code 0
          :stdout "PREFIX 1,2,Ok\nMATCH FOO\nEND\n"
          :stderr ""
          :default-directory "/"
          :process-environment
          '("RUST_SRC_PATH=/home/user/src/rustc-1.10.0/src"
            "CARGO_HOME=/home/user/.cargo"))))
    (racer-debug)))

(ert-deftest racer--relative ()
  ;; Common case: the path is relative to the directory.
  (should (equal (racer--relative "/foo/bar" "/foo")
                 "./bar"))
  ;; Path is not relative, but it's a home directory.
  (should (equal (racer--relative (f-expand "~/foo")
                                  (f-expand "~/bar"))
                 "~/foo"))
  ;; Path is not relative and not a home directory.
  (should (equal (racer--relative "/foo/bar" "/quux")
                 "/foo/bar")))

(ert-deftest racer-eldoc-no-completions ()
  "`racer-eldoc' should handle no completions gracefully."
  (cl-letf (((symbol-function 'racer--call)
             (lambda (&rest _)
               "PREFIX 4,4,\nEND\n")))
    (with-temp-buffer
      (rust-mode)
      (insert "use ")
      ;; Midle of the 'use'.
      (goto-char 2)
      ;; Should return nil without crashing.
      (should (null (racer-eldoc))))))

(ert-deftest racer-complete--extract-args ()
  (should
   (equal
    (racer-complete--extract-args
     "pub unsafe fn alloc(layout: Layout) -> *mut u8")
    "(layout: Layout)")))
