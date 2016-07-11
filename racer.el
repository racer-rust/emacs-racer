;;; racer.el --- Rust completion and code navigation via racer

;; Copyright (c) 2014 Phil Dawes

;; Author: Phil Dawes
;; URL: https://github.com/racer-rust/emacs-racer
;; Version: 1.1
;; Package-Requires: ((emacs "24.3") (rust-mode "0.2.0") (dash "2.11.0") (s "1.10.0"))
;; Keywords: abbrev, convenience, matching, rust, tools

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any
;; person obtaining a copy of this software and associated
;; documentation files (the "Software"), to deal in the
;; Software without restriction, including without
;; limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of
;; the Software, and to permit persons to whom the Software
;; is furnished to do so, subject to the following
;; conditions:

;; The above copyright notice and this permission notice
;; shall be included in all copies or substantial portions
;; of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
;; ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
;; TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
;; SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
;; IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; You will need to configure Emacs to find racer:
;;
;; (setq racer-rust-src-path "<path-to-rust-srcdir>/src/")
;; (setq racer-cmd "<path-to-racer>/target/release/racer")
;;
;; To activate racer in Rust buffers, run:
;;
;; (add-hook 'rust-mode-hook #'racer-mode)
;;
;; You can also use racer to find definition at point via
;; `racer-find-definition', bound to `M-.' by default.
;;
;; Finally, you can also use Racer to show the signature of the
;; current function in the minibuffer:
;;
;; (add-hook 'racer-mode-hook #'eldoc-mode)

;;; Code:

(require 'dash)
(require 'etags)
(require 'rust-mode)
(require 's)
(require 'thingatpt)

(defgroup racer nil
  "Support for Rust completion via racer."
  :link '(url-link "https://github.com/racer-rust/emacs-racer/")
  :group 'rust-mode)

(defcustom racer-cmd
  (or (executable-find "racer") "/usr/local/bin/racer")
  "Path to the racer binary."
  :type 'file
  :group 'racer)

(defcustom racer-rust-src-path
  (or (getenv "RUST_SRC_PATH") "/usr/local/src/rust/src")
  "Path to the rust source tree."
  :type 'file
  :group 'racer)

(defcustom racer-cargo-home
  (or (getenv "CARGO_HOME") (concat (getenv "HOME") "/.cargo"))
  "To enable completion for cargo crates, you need to set the CARGO_HOME environment variable to .cargo in your home directory."
  :type 'file
  :group 'racer
  )

(defun racer--cargo-project-root ()
  "Find the root of the current Cargo project."
  (let ((root (locate-dominating-file (or buffer-file-name default-directory) "Cargo.toml")))
    (and root (file-truename root))))

(defun racer--call (command &rest args)
  "Call racer command COMMAND with args ARGS."
  (setenv "RUST_SRC_PATH" (expand-file-name racer-rust-src-path))
  (setenv "CARGO_HOME" (expand-file-name racer-cargo-home))
  (let ((default-directory (racer--cargo-project-root)))
    (apply #'process-lines racer-cmd command args)
    ))

(defun racer--call-at-point (command)
  "Call racer command COMMAND at point of current buffer."
  (let ((tmp-file (make-temp-file "racer")))
    (write-region nil nil tmp-file nil 'silent)
    (prog1 (racer--call command
                        (number-to-string (line-number-at-pos))
                        (number-to-string (current-column))
                        (buffer-file-name)
                        tmp-file)
      (delete-file tmp-file))))

(defun racer-complete-at-point ()
  "Complete the symbol at point."
  (unless (nth 3 (syntax-ppss)) ;; not in string
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (beg (or (car bounds) (point)))
           (end (or (cdr bounds) (point))))
      (list beg end
            (completion-table-dynamic #'racer-complete)
            :annotation-function #'racer-complete--annotation
            :company-prefix-length (racer-complete--prefix-p beg end)
            :company-docsig #'racer-complete--docsig
            :company-location #'racer-complete--location))))

(defun racer-complete (&optional _ignore)
  "Completion candidates at point."
  (->> (racer--call-at-point "complete")
       (--filter (s-starts-with? "MATCH" it))
       (--map (-let [(name line col file matchtype ctx)
                     (s-split-up-to "," (s-chop-prefix "MATCH " it) 5)]
                (put-text-property 0 1 'line (string-to-number line) name)
                (put-text-property 0 1 'col (string-to-number col) name)
                (put-text-property 0 1 'file file name)
                (put-text-property 0 1 'matchtype matchtype name)
                (put-text-property 0 1 'ctx ctx name)
                name))))

(defun racer--trim-up-to (needle s)
  "Return content after the occurrence of NEEDLE in S."
  (-if-let (idx (s-index-of needle s))
      (substring s (+ idx (length needle)))
    s))

(defun racer-complete--prefix-p (beg end)
  "Return t if a completion should be triggered for a prefix between BEG and END."
  (save-excursion
    (goto-char beg)
    ;; If we're at the beginning of the buffer, we can't look back 2
    ;; characters.
    (ignore-errors
      (looking-back "\\.\\|::" 2))))

(defun racer-complete--annotation (arg)
  "Return an annotation for completion candidate ARG."
  (let* ((ctx (get-text-property 0 'ctx arg))
         (type (get-text-property 0 'matchtype arg))
         (pretty-ctx
          (pcase type
            ("Module"
             (if (string= arg ctx)
                 ""
               (concat " " ctx)))
            (_
             (->> ctx
                  (racer--trim-up-to arg)
                  (s-chop-suffixes '(" {" "," ";")))))))
    (format "%s : %s" pretty-ctx type)))

(defun racer-complete--docsig (arg)
  "Return a signature for completion candidate ARG."
  (racer--syntax-highlight (format "%s" (get-text-property 0 'ctx arg))))

(defun racer-complete--location (arg)
  "Return location of completion candidate ARG."
  (cons (get-text-property 0 'file arg)
        (get-text-property 0 'line arg)))

;;;###autoload
(defun racer-find-definition ()
  "Run the racer find-definition command and process the results."
  (interactive)
  (-if-let (match (--first (s-starts-with? "MATCH" it)
                           (racer--call-at-point "find-definition")))
      (-let [(_name line col file _matchtype _ctx)
             (s-split-up-to "," (s-chop-prefix "MATCH " match) 5)]
        (if (fboundp 'xref-push-marker-stack)
            (xref-push-marker-stack)
          (with-no-warnings
            (ring-insert find-tag-marker-ring (point-marker))))
        (find-file file)
        (goto-char (point-min))
        (forward-line (1- (string-to-number line)))
        (forward-char (string-to-number col)))
    (error "No definition found")))

(defun racer--syntax-highlight (str)
  "Apply font-lock properties to a string STR of Rust code."
  (with-temp-buffer
    (insert str)
    (delay-mode-hooks (rust-mode))
    (if (fboundp 'font-lock-ensure)
        (font-lock-ensure)
      (with-no-warnings
        (font-lock-fontify-buffer)))
    (buffer-string)))

(defun racer--goto-func-name ()
  "If point is inside a function call, move to the function name.

foo(bar, |baz); -> foo|(bar, baz);"
  (let ((last-paren-pos (nth 1 (syntax-ppss)))
        (start-pos (point)))
    (when last-paren-pos
      ;; Move to just before the last paren.
      (goto-char last-paren-pos)
      ;; If we're inside a round paren, we're inside a function call.
      (unless (looking-at "(")
        ;; Otherwise, return to our start position, as point may have been on a
        ;; function already:
        ;; foo|(bar, baz);
        (goto-char start-pos)))))

(defun racer-eldoc ()
  "Show eldoc for context at point."
  (save-excursion
    (racer--goto-func-name)
    ;; If there's a variable at point:
    (-when-let (rust-sym (symbol-at-point))
      (-some->>
       ;; then look at the current completion possiblities,
       (racer-complete)
       ;; extract the possibility that matches this symbol exactly
       (--filter (string= it (symbol-name rust-sym)))
       (-first-item)
       ;; and return the prototype that Racer gave us.
       (get-text-property 0 'ctx)
       ;; Finally, apply syntax highlighting for the minibuffer.
       (racer--syntax-highlight)))))

(defvar racer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") #'racer-find-definition)
    (define-key map (kbd "M-,") #'pop-tag-mark)
    map))

;;;###autoload
(define-minor-mode racer-mode
  "Minor mode for racer."
  :lighter " racer"
  :keymap racer-mode-map
  (setq-local eldoc-documentation-function #'racer-eldoc)
  (set (make-local-variable 'completion-at-point-functions) nil)
  (add-hook 'completion-at-point-functions #'racer-complete-at-point))

(define-obsolete-function-alias 'racer-turn-on-eldoc 'eldoc-mode)
(define-obsolete-function-alias 'racer-activate 'racer-mode)

(provide 'racer)
;;; racer.el ends here
