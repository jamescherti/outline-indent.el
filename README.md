# outline-indent.el - Make Emacs Fold Text Using Indentation Levels
![Build Status](https://github.com/jamescherti/outline-indent.el/actions/workflows/ci.yml/badge.svg)
[![MELPA](https://melpa.org/packages/outline-indent-badge.svg)](https://melpa.org/#/outline-indent)
[![MELPA Stable](https://stable.melpa.org/packages/outline-indent-badge.svg)](https://stable.melpa.org/#/outline-indent)
![License](https://img.shields.io/github/license/jamescherti/outline-indent.el)
![](https://raw.githubusercontent.com/jamescherti/outline-indent.el/main/.images/made-for-gnu-emacs.svg)

The **outline-indent** Emacs package provides a minor mode that enables code folding based on indentation levels.

The **outline-indent** package is a fast and reliable alternative to the **origami.el** and **yafolding.el** packages. (*origami.el* and *yafolding.el* are no longer maintained, slow, and known to have bugs that impact their reliability and performance.)

In addition to code folding, *outline-indent* allows:
- moving indented blocks up and down with `(outline-indent-move-subtree-up)` and `(outline-indent-move-subtree-down)`,
- indenting/unindenting to adjust indentation levels with `(outline-indent-shift-right)` and `(outline-indent-shift-left)`,
- inserting a new line with the same indentation level as the current line with `(outline-indent-insert-heading)`,
- Move backward/forward to the indentation level of the current line with `(outline-indent-backward-same-level)` and `(outline-indent-forward-same-level)`.
- Customizing the ellipsis to replace the default "..." with something more visually appealing, such as "▼".
- Selecting the indented block with `(outline-indent-select)`.
- Toggle the visibility of the indentation level under the cursor: `(outline-indent-toggle-level-at-point)`
- and other features.

The *outline-indent* package uses the built-in *outline-minor-mode*, which is *maintained by the Emacs developers* and is less likely to be abandoned like *origami.el* or *yafolding.el*. Since *outline-indent* is based on *outline-minor-mode*, it's also much **much faster** than *origami.el* and *yafolding.el*.

![](https://raw.githubusercontent.com/jamescherti/outline-indent.el/main/.screenshot.png)
*(The Emacs theme in the screenshot above is the [tomorrow-night-deepblue-theme](https://github.com/jamescherti/tomorrow-night-deepblue-theme.el))*

![](https://raw.githubusercontent.com/jamescherti/outline-indent.el/main/.screenshot2.png)

The *outline-indent* Emacs package offers a similar functionality to Vim's `set foldmethod=indent` setting. Just as in Vim, it allows to fold and unfold code sections based on their indentation levels.

## Table of Contents
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
## Table of Contents

- [outline-indent.el - Make Emacs Fold Text Using Indentation Levels](#outline-indentel---make-emacs-fold-text-using-indentation-levels)
  - [Table of Contents](#table-of-contents)
  - [Installation](#installation)
  - [Activation](#activation)
    - [Manual activation](#manual-activation)
    - [Automatic activation using hooks](#automatic-activation-using-hooks)
    - [Adjusting the shift width and default offset](#adjusting-the-shift-width-and-default-offset)
  - [Usage](#usage)
    - [How to check if it is working?](#how-to-check-if-it-is-working)
    - [Functions specific to outline-indent-minor-mode](#functions-specific-to-outline-indent-minor-mode)
      - [Managing folds](#managing-folds)
      - [Selecting indented text](#selecting-indented-text)
      - [outline-indent-backward-same-level and outline-indent-forward-same-level](#outline-indent-backward-same-level-and-outline-indent-forward-same-level)
      - [outline-indent-shift-left and outline-indent-shift-right](#outline-indent-shift-left-and-outline-indent-shift-right)
      - [outline-indent-move-subtree-up and outline-indent-move-subtree-down](#outline-indent-move-subtree-up-and-outline-indent-move-subtree-down)
      - [outline-indent-insert-heading](#outline-indent-insert-heading)
    - [Vanilla Emacs](#vanilla-emacs)
    - [Evil mode](#evil-mode)
  - [Frequently asked questions](#frequently-asked-questions)
    - [Maintaining blank lines between folded sections](#maintaining-blank-lines-between-folded-sections)
    - [How to Prevent Emacs from Searching Folded Sections](#how-to-prevent-emacs-from-searching-folded-sections)
    - [Why not use origami.el or yafolding?](#why-not-use-origamiel-or-yafolding)
    - [Why not use folding.el?](#why-not-use-foldingel)
    - [How to make Emacs indent new lines based on previous non-blank line?](#how-to-make-emacs-indent-new-lines-based-on-previous-non-blank-line)
    - [What other packages can be used to maintain proper indentation in indentation-sensitive programming languages?](#what-other-packages-can-be-used-to-maintain-proper-indentation-in-indentation-sensitive-programming-languages)
      - [Displaying vertical indentation guide bars](#displaying-vertical-indentation-guide-bars)
      - [Detecting indentation](#detecting-indentation)
    - [How to view different outline-indent folds in separate windows?](#how-to-view-different-outline-indent-folds-in-separate-windows)
  - [License](#license)
  - [Links](#links)

<!-- markdown-toc end -->

## Installation

To install *outline-indent* from MELPA:

1. If you haven't already done so, [add MELPA repository to your Emacs configuration](https://melpa.org/#/getting-started).

2. Add the following code to your Emacs init file to install *outline-indent* from MELPA:
``` emacs-lisp
(use-package outline-indent
  :ensure t
  :defer t
  :commands outline-indent-minor-mode
  :custom
  (outline-indent-ellipsis " ▼ "))
```

## Activation

### Manual activation

Once installed, the minor mode can be activated using:
``` emacs-lisp
(outline-indent-minor-mode)
```

### Automatic activation using hooks

The minor mode can also be automatically activated for a certain modes. For example for Python and YAML:
``` emacs-lisp
;; Python
(add-hook 'python-mode-hook #'outline-indent-minor-mode)
(add-hook 'python-ts-mode-hook #'outline-indent-minor-mode)

;; YAML
(add-hook 'yaml-mode-hook #'outline-indent-minor-mode)
(add-hook 'yaml-ts-mode-hook #'outline-indent-minor-mode)
```

### Adjusting the shift width and default offset

You can adjust the `outline-indent-shift-width` and `outline-indent-default-offset` according to your preferences. While the default value of 1 is adequate for most modes, setting the appropriate value ensures that the promote and demote functions correctly adjust the indentation of blocks. For example:
``` emacs-lisp
;; Python
(dolist (hook '(python-mode python-ts-mode-hook))
  (add-hook hook #'(lambda()
                     (setq-local outline-indent-default-offset 4)
                     (setq-local outline-indent-shift-width 4))))

;; YAML
(dolist (hook '(yaml-mode yaml-ts-mode-hook))
  (add-hook hook #'(lambda()
                     (setq-local outline-indent-default-offset 2)
                     (setq-local outline-indent-shift-width 2)))
```

Explanation:
1. **Outline Indentation Parameters**:
   - **`outline-indent-default-offset`**: This variable determines the base indentation level for each outline level. It specifies the amount by which each successive outline level should be indented, effectively controlling the visual structure of the outline.
   - **`outline-indent-shift-width`**: This variable determines the number of spaces by which to adjust the indentation when promoting or demoting an indented block with `(outline-indent-shift-right)` and `(outline-indent-shift-left)`.

2. **Why Customize These Values?**:
   - **Language-specific Indentation**: Different programming languages and file formats have different indentation standards. Python typically uses 4 spaces per indentation level, while YAML often uses 2 spaces. Customizing these values for different modes ensures that your outline structure is consistent with the language's indentation practices.
   - **Shift right and shift left**: When you use `outline-indent-shift-left` and `outline-indent-shift-right` functions, these settings control how much the outline level is adjusted. For instance, in Python mode, promoting a block of code (moving it to a higher outline level) will decrease its indentation by 4 spaces, and demoting it will increase its indentation by 4 spaces.

3. **Default Behavior**:
   - By default, `outline-indent-default-offset` is set to 1, which works with any indentation level, as even a single space is enough to fold any indented block using *outline-indent*.
   - By default, `outline-indent-default-shift-width` is `nil`, which means it inherits the value of `outline-indent-default-offset`. If you do not explicitly set `outline-indent-shift-width`, the promote and demote operations will use the same value as the offset. This default behavior works well in many cases, but fine-tuning these values can be necessary for languages or formats with specific indentation needs.

## Usage

### How to check if it is working?

Run the following function to fold all indented blocks:

``` emacs-lisp
(outline-indent-close-folds)
```

### Functions specific to outline-indent-minor-mode

#### Managing folds

These functions help you manage the visibility of code blocks or headings in `outline-indent-minor-mode`. Use them to control which sections of your document are visible or hidden:

Fold at point:
- `(outline-indent-open-fold)`: Open fold at point.
- `(outline-indent-close-fold)`: Close fold at point.

All folds:
- `(outline-indent-open-folds)`: Open all folds.
- `(outline-indent-close-folds)`: Close all folds.

Other:
- `(outline-indent-open-fold-rec)`: Open fold at point recursively.

Toggle:

- `(outline-indent-toggle-level-at-point)`: Toggle the visibility of the indentation level under the cursor.
- `(outline-indent-toggle-fold)`: Open or close a fold under point.

#### Selecting indented text

The current indented text can be selected using:
```elisp
(outline-indent-select)
```

#### outline-indent-backward-same-level and outline-indent-forward-same-level

*(By default, `outline-indent-advise-outline-functions` is set to t, which means that you can also use the built-in outline functions `(outline-backward-same-level)` and `(outline-forward-same-level)` as an alternative to `(outline-indent-backward-same-level)` and `(outline-indent-forward-same-level)`)*

To move to the next block with the same indentation level:

``` emacs-lisp
(outline-indent-forward-same-level)
```

To move to the previous block with the same indentation level:

``` emacs-lisp
(outline-indent-backward-same-level)
```

#### outline-indent-shift-left and outline-indent-shift-right

*(By default, `outline-indent-advise-outline-functions` is set to t, which means that you can also use the built-in outline functions `(outline-promote)` and `(outline-demote)` as an alternative to `(outline-indent-shift-left)` and `(outline-indent-shift-right)`)*

These functions can be used to decrease and increase the indentation level of indented blocks.

To increase indentation:
``` emacs-lisp
(outline-indent-shift-right)
```

To decrease indentation:
``` emacs-lisp
(outline-indent-shift-left)
```

The global variable `outline-indent-shift-width` is used to determine the number of spaces to indent or unindent the subtree.

#### outline-indent-move-subtree-up and outline-indent-move-subtree-down

*(By default, `outline-indent-advise-outline-functions` is set to t, which means that you can also use the built-in outline functions `(outline-move-subtree-up)` and `(outline-move-subtree-down)`, as an alternative to `(outline-indent-move-subtree-up)` and `(outline-indent-move-subtree-down)`)*

These functions can be used to move the current subtree down past ARGS headlines of the same level.

To move the subtree down, use:
``` emacs-lisp
(outline-indent-move-subtree-down)
```

To move the subtree up, use:
``` emacs-lisp
(outline-indent-move-subtree-up)
```

#### outline-indent-insert-heading

*(By default, `outline-indent-advise-outline-functions` is set to t, which means that you can also use the built-in outline function `outline-insert-heading` as an alternative to `outline-indent-insert-heading`)*

The `(outline-indent-insert-heading)` function inserts a new line with the same indentation level/depth as the current line just before the next heading that shares the same or less indentation level. It finds the nearest non-empty line with the same or less indentation as the current line and inserts a new line before it.

In `outline-indent-minor-mode`, where most lines are treated as headings, this function is suitable for maintaining consistent indentation within the outline structure. It can be used as an alternative to `outline-insert-heading` to insert content at the same indentation level after the current fold.

Example usage:
``` emacs-lisp
(outline-indent-insert-heading)
```

### Vanilla Emacs

Use the standard `outline-mode`/`outline-minor-mode` commands to fold and unfold sections of your indented file:
- `(hide-sublevels 1)`: Fold all folds.
- `(outline-hide-body)`: Hide all body lines in buffer, leaving all headings visible.
- `(outline-hide-other)`: Hide everything except current body and parent and top-level headings.
- `(outline-hide-entry)`: Hide the body directly following this heading.
- `(outline-hide-leaves)`: Hide the body after this heading and at deeper levels.
- `(outline-hide-subtree)`: Hide everything after this heading at deeper levels.
- `(outline-show-children)`: Show all direct subheadings of this heading.
- `(outline-hide-sublevels)`: Hide everything but the top LEVELS levels of headers, in whole buffer.
- `(outline-show-all)`: Show all of the text in the buffer.
- `(outline-show-entry)`: Show the body directly following this heading.
- `(outline-show-subtree)`: Show everything after this heading at deeper levels.
- `(outline-show-branches)`: Show all subheadings of this heading, but not their bodies.
- `(outline-show-children)`: Show all direct subheadings of this heading.

You can also indent/unindent and move subtree up and down using:

- `(outline-backward-same-level)` and `(outline-forward-same-level)`: Move backward/forward to the indentation level of the current line.
- `(outline-indent-shift-right)` and `(outline-indent-shift-left)`: Indent or unindent the entire subtree.
- `(outline-indent-move-subtree-down)` and `(outline-indent-move-subtree-up)` to move the current subtree up or down.
- `(outline-insert-heading)` to insert a new line with the same indentation level/depth as the current line just before the next heading that shares the same or less indentation level.

Move to the next and previous visible fold:
- `outline-previous-visible-heading`
- `outline-next-visible-heading`

Move forward or backward to the same indentation level:
- `outline-forward-same-level`: Move forward to the same indentation level as the one under the cursor.
- `outline-backward-same-level`: Move backward to the the same indentation level as as the one under the cursor.

### Evil mode

In Evil mode, *outline-indent* works out of the box if you install `evil-collection`, and you can use the Evil and evil-collection keyboard mappings:
- Open fold(s): `zo`, `zO`, `zr`
- Close fold(s): `zc`, `zC`, `zM`
- Toggle folds: `za`
- Next visible fold/heading: `]]` and `[[`
- Move forward/backward to the same indentation level: `gj` and `gk`

You may want to set a few additional key mappings:
```emacs-lisp
(with-eval-after-load "evil"
  (defun my-evil-define-key-outline-indent-minor-mode ()
    ;; Set `M-h` and `M-l` to decrease and increase the indentation level of
    ;; indented blocks
    (evil-define-key 'normal 'local (kbd "M-h") #'outline-indent-shift-left)
    (evil-define-key 'normal 'local (kbd "M-l") #'outline-indent-shift-right)

    ;; Set `M-k` and `M-j` to move indented blocks up and down
    (evil-define-key 'normal 'local (kbd "M-k") #'outline-indent-move-subtree-up)
    (evil-define-key 'normal 'local (kbd "M-j") #'outline-indent-move-subtree-down)

    (unless (derived-mode-p 'prog-mode)
      ;; In prog-mode, [[, ]], gj, and gk provide navigation to the previous
      ;; and next function, so there is no need to override them.
      (evil-define-key 'normal 'local (kbd "]]") #'outline-indent-forward-same-level)
      (evil-define-key 'normal 'local (kbd "[[") #'outline-indent-backward-same-level)
      (evil-define-key 'normal 'local (kbd "gj") #'outline-indent-forward-same-level)
      (evil-define-key 'normal 'local (kbd "gk") #'outline-indent-backward-same-level))

    (evil-define-key 'normal 'local (kbd "gV") #'outline-indent-select)

    ;; Set C-<return> to insert a new line with the same indentation
    ;; level/depth as the current line just before the next heading
    (evil-define-key '(normal insert) 'local (kbd "C-<return>")
      (defun my-evil-outline-indent-insert-heading ()
        (interactive)
        (outline-indent-insert-heading)
        (evil-insert-state))))

  (add-hook 'outline-indent-minor-mode-hook
            #'my-evil-define-key-outline-indent-minor-mode))
```

## Frequently asked questions

### Maintaining blank lines between folded sections

The `outline-blank-line` variable can be set to `t` (true) to maintain blank lines between folded sections, making it easier to distinguish between folds:

``` emacs-lisp
(setq outline-blank-line t)
```

### How to Prevent Emacs from Searching Folded Sections

To prevent Emacs from searching within folded sections, set `search-invisible` to `nil` by adding the following line to your Emacs init file:
```emacs-lisp
(setq-default search-invisible nil)
```

This setting ensures that Emacs skips invisible or folded text during searches, so hidden sections are not included in the search results.

### Why not use origami.el or yafolding?

The `origami.el` and `yafolding.el` package are not reliable method for folding indented code because they are:
- No longer maintained (abandoned),
- Slow,
- Known to have bugs that affect their reliability and performance.

On the other hand, `outline-indent` leverages the built-in `outline-minor-mode`, which is:
- Fast,
- Actively maintained by the Emacs developers.

### Why not use folding.el?

The `folding.el` package is no longer maintained (abandoned) and uses markers in the buffer to annotate folds. It does not support using indentation levels to determine foldable sections.

In contrast, `outline-indent` uses indentation levels to determine foldable sections.

### How to make Emacs indent new lines based on previous non-blank line?

The following code snippet configures Emacs to indent based on the indentation of the previous non-blank line:
```emacs-lisp
;; This ensures that pressing Enter will insert a new line and indent it.
(global-set-key (kbd "RET") #'newline-and-indent)

;; Indentation based on the indentation of the previous non-blank line.
(setq-default indent-line-function #'indent-relative-first-indent-point)

;; In modes such as `text-mode', pressing Enter multiple times removes
;; the indentation. The following fixes the issue and ensures that text
;; is properly indented using `indent-relative' or
;; `indent-relative-first-indent-point'.
(setq-default indent-line-ignored-functions '())
```

### What other packages can be used to maintain proper indentation in indentation-sensitive programming languages?

#### Displaying vertical indentation guide bars

Choose one of these packages that are available on MELPA:
- highlight-indentation
- highlight-indent-guides

(There is also indent-bars, but it is not yet available on MELPA.)

#### Detecting indentation

The *dtrt-indent* package automatically detects the indentation offset used in source code files and adjusts Emacs settings to match, simplifying the editing of files with varying indentation styles. To install it, add the following to your Emacs init file:
```emacs-lisp
(use-package dtrt-indent
  :ensure t
  :commands (dtrt-indent-global-mode
             dtrt-indent-mode
             dtrt-indent-adapt
             dtrt-indent-undo
             dtrt-indent-diagnosis
             dtrt-indent-highlight)
  :config
  (dtrt-indent-global-mode))
```

### How to view different outline-indent folds in separate windows?

You can use indirect buffers, a feature that allow multiple views of the same underlying data in separate windows.

Indirect buffers are useful when working with outline-indent folds where you might want to focus on different sections of a document simultaneously, without altering the view in other windows.

For example, one window might display a fully expanded view (original buffer), while another window (the indirect buffer) shows only specific folds or indentation levels, allowing you to compare or edit sections side by side.

To create an indirect buffer of the current buffer, you can use the following function:

``` emacs-lisp
(clone-indirect-buffer nil t)
```

## License

The *outline-indent* Emacs package has been written by [James Cherti](https://www.jamescherti.com/) and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version.

Copyright (C) 2024-2025 [James Cherti](https://www.jamescherti.com)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.

## Links

- [outline-indent @GitHub](https://github.com/jamescherti/outline-indent.el)
- [outline-indent @MELPA](https://melpa.org/#/outline-indent)
- Article: [outline-indent – Indentation based Folding and Outlining in Emacs](https://www.jamescherti.com/fold-outline-indentation-emacs-package/)

Other Emacs packages by the same author:
- [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d): This repository hosts a minimal Emacs configuration designed to serve as a foundation for your vanilla Emacs setup and provide a solid base for an enhanced Emacs experience.
- [compile-angel.el](https://github.com/jamescherti/compile-angel.el): **Speed up Emacs!** This package guarantees that all .el files are both byte-compiled and native-compiled, which significantly speeds up Emacs.
- [easysession.el](https://github.com/jamescherti/easysession.el): Easysession is lightweight Emacs session manager that can persist and restore file editing buffers, indirect buffers/clones, Dired buffers, the tab-bar, and the Emacs frames (with or without the Emacs frames size, width, and height).
- [vim-tab-bar.el](https://github.com/jamescherti/vim-tab-bar.el): Make the Emacs tab-bar Look Like Vim’s Tab Bar.
- [elispcomp](https://github.com/jamescherti/elispcomp): A command line tool that allows compiling Elisp code directly from the terminal or from a shell script. It facilitates the generation of optimized .elc (byte-compiled) and .eln (native-compiled) files.
- [tomorrow-night-deepblue-theme.el](https://github.com/jamescherti/tomorrow-night-deepblue-theme.el): The Tomorrow Night Deepblue Emacs theme is a beautiful deep blue variant of the Tomorrow Night theme, which is renowned for its elegant color palette that is pleasing to the eyes. It features a deep blue background color that creates a calming atmosphere. The theme is also a great choice for those who miss the blue themes that were trendy a few years ago.
- [Ultyas](https://github.com/jamescherti/ultyas/): A command-line tool designed to simplify the process of converting code snippets from UltiSnips to YASnippet format.
- [dir-config.el](https://github.com/jamescherti/dir-config.el): Automatically find and evaluate .dir-config.el Elisp files to configure directory-specific settings.
- [flymake-bashate.el](https://github.com/jamescherti/flymake-bashate.el): A package that provides a Flymake backend for the bashate Bash script style checker.
- [flymake-ansible-lint.el](https://github.com/jamescherti/flymake-ansible-lint.el): An Emacs package that offers a Flymake backend for ansible-lint.
- [inhibit-mouse.el](https://github.com/jamescherti/inhibit-mouse.el): A package that disables mouse input in Emacs, offering a simpler and faster alternative to the disable-mouse package.
- [quick-sdcv.el](https://github.com/jamescherti/quick-sdcv.el): This package enables Emacs to function as an offline dictionary by using the sdcv command-line tool directly within Emacs.
- [enhanced-evil-paredit.el](https://github.com/jamescherti/enhanced-evil-paredit.el): An Emacs package that prevents parenthesis imbalance when using *evil-mode* with *paredit*. It intercepts *evil-mode* commands such as delete, change, and paste, blocking their execution if they would break the parenthetical structure.
- [stripspace.el](https://github.com/jamescherti/stripspace.el): Ensure Emacs Automatically removes trailing whitespace before saving a buffer, with an option to preserve the cursor column.
- [persist-text-scale.el](https://github.com/jamescherti/persist-text-scale.el): Ensure that all adjustments made with text-scale-increase and text-scale-decrease are persisted and restored across sessions.
- [pathaction.el](https://github.com/jamescherti/pathaction.el): Execute the pathaction command-line tool from Emacs. The pathaction command-line tool enables the execution of specific commands on targeted files or directories. Its key advantage lies in its flexibility, allowing users to handle various types of files simply by passing the file or directory as an argument to the pathaction tool. The tool uses a .pathaction.yaml rule-set file to determine which command to execute. Additionally, Jinja2 templating can be employed in the rule-set file to further customize the commands.
