# Racer for Emacs
[![MELPA](http://melpa.org/packages/racer-badge.svg)](http://melpa.org/#/racer)
[![MELPA Stable](http://stable.melpa.org/packages/racer-badge.svg)](http://stable.melpa.org/#/racer)

This is the official Emacs package for
[Racer](http://github.com/phildawes/racer).

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [Racer for Emacs](#racer-for-emacs)
    - [Completion](#completion)
    - [Find Definitions](#find-definitions)
    - [Describe Functions and Types](#describe-functions-and-types)
    - [Installation](#installation)
    - [Tests](#tests)

<!-- markdown-toc end -->

## Completion

racer.el supports code completion of variables, functions and modules.

![racer completion screenshot](images/racer_completion.png)

You can also press <kbd>F1</kbd> to pop up a help buffer for the current
completion candidate.

## Find Definitions

racer.el can jump to definition of functions and types.

![racer go to definition](images/racer_goto.gif)

You can use <kbd>M-.</kbd> to go to the definition, and <kbd>M-,</kbd>
to go back.

## Describe Functions and Types

racer.el can show a help buffer based on the docstring of the thing at
point.

![racer completion screenshot](images/racer_help.png)

Use <kbd>M-x racer-describe</kbd> to open the help buffer.

## Installation

1. Build / Install [Racer](http://github.com/phildawes/racer)

1. Install emacs 24.

2. Allow Emacs to install packages from MELPA:

   ```el
   (require 'package)
   (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
   ```

2. Install racer: `M-x package-list` Find the racer package and install it

3. If racer is not in the path, configure emacs to find your racer binary and rust source directory
   ```el
   (setq racer-cmd "<path-to-racer-srcdir>/target/release/racer")
   (setq racer-rust-src-path "<path-to-rust-srcdir>/src/")
   ```

4. Configure Emacs to activate racer when rust-mode starts:
   ```el
   (add-hook 'rust-mode-hook #'racer-mode)
   (add-hook 'racer-mode-hook #'eldoc-mode)
   ```

   For completions, install company with `M-x package-install RET company`. A sample configuration:
   ```el

   (add-hook 'racer-mode-hook #'company-mode)

   (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
   (setq company-tooltip-align-annotations t)
   ```
   For automatic completions, customize `company-idle-delay` and `company-minimum-prefix-length`.

5. Open a rust file and try typing ```use std::io::B``` and press `<tab>`

6. Place your cursor over a symbol and hit `M-.` to jump to the
definition.

7. Hit `M-,` to jump back to the symbol usage location.

## Tests

racer.el includes tests. To run them, you need to install
[Cask](https://github.com/cask/cask), then:

```
$ cask install
$ cask exec ert-runner
```
