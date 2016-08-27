# v1.2 (unrelased)

* We now explicitly try `~/.cargo/bin/racer` if `racer isn't on path.

# v1.1

* Fixed a crash when point is at the beginning of buffer.
* Fixed a crash when not in a cargo project.
* Added `racer-cargo-home`, which enables completion for cargo crates.
* Various improvements to formatting of completion candidates.
* Added `racer-describe`.

# v1.0.2

* Trigger completions after `::` or `.`.
* Compatibility with latest company
* Fixed an issue where TAGS from other projects were also completion
  candidates

# v1.0.1

No changes since v0.0.2.

This release was created to [work around an issue
where MELPA stable](https://github.com/milkypostman/melpa/issues/3205)
had created a v1.0.0 from an early version of racer.el

# v0.0.2

Initial release. Includes:

* Code completion with company
* Jump to definition
* Eldoc

Early users who are using `racer-activate` or `racer-turn-on-eldoc`
should use `racer-mode` and `eldoc-mode` instead. The former have been
deprecated.
