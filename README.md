# outline-indent.el - Indentation-Based Code Folding for Emacs, a Modern Replacement for origami.el and yafolding.el
![Build Status](https://github.com/jamescherti/outline-indent.el/actions/workflows/ci.yml/badge.svg)
[![MELPA](https://melpa.org/packages/outline-indent-badge.svg)](https://melpa.org/#/outline-indent)
[![MELPA Stable](https://stable.melpa.org/packages/outline-indent-badge.svg)](https://stable.melpa.org/#/outline-indent)
![License](https://img.shields.io/github/license/jamescherti/outline-indent.el)
![](https://raw.githubusercontent.com/jamescherti/outline-indent.el/main/.images/made-for-gnu-emacs.svg)

The **outline-indent** Emacs package provides a minor mode that enables indentation-based code folding. It is highly efficient and leverages built-in Emacs functionality to perform folding operations.

The **outline-indent** package is a modern replacement for legacy packages such as **origami.el** and **yafolding.el**. (*Both **origami.el** and **yafolding.el** are unmaintained, suffer from performance issues, and contain known bugs that undermine their reliability.*)

In addition to code folding, *outline-indent* allows:
- moving indented blocks up and down with `(outline-indent-move-subtree-up)` and `(outline-indent-move-subtree-down)`,
- indenting/unindenting to adjust indentation levels with `(outline-indent-shift-right)` and `(outline-indent-shift-left)`,
- inserting a new line with the same indentation level as the current line with `(outline-indent-insert-heading)`,
- Move backward/forward to the indentation level of the current line with `(outline-indent-backward-same-level)` and `(outline-indent-forward-same-level)`.
- Customizing the ellipsis to replace the default "..." with something more visually appealing, such as "▼".
- Selecting the indented block with `(outline-indent-select)`.
- Toggle the visibility of the indentation level under the cursor: `(outline-indent-toggle-level-at-point)`
- The `outline-indent-minor-mode` mode automatically detects the current major mode's indentation settings to determine the *basic offset*, which sets the indentation for each outline level, and the *shift width* used for promoting or demoting blocks. This ensures consistent outline indentation without manual configuration.
- and other features.

The *outline-indent* package uses the built-in *outline-minor-mode*, which is *maintained by the Emacs developers* and is less likely to be abandoned like *origami.el* or *yafolding.el*. Since *outline-indent* is based on *outline-minor-mode*, it's also much **much faster** than *origami.el* and *yafolding.el*.

If this package enhances your workflow, please consider **starring outline-indent** on GitHub.

![](https://raw.githubusercontent.com/jamescherti/outline-indent.el/main/.images/screenshot.png)
*(The Emacs theme in the screenshot above is the [tomorrow-night-deepblue-theme](https://github.com/jamescherti/tomorrow-night-deepblue-theme.el))*

![](https://raw.githubusercontent.com/jamescherti/outline-indent.el/main/.images/screenshot2.png)

The *outline-indent* Emacs package offers a similar functionality to Vim's `set foldmethod=indent` setting. Just as in Vim, it allows to fold and unfold code sections based on their indentation levels.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
## Table of Contents

- [outline-indent.el - Indentation-Based Code Folding for Emacs, a Modern Replacement for origami.el and yafolding.el](#outline-indentel---indentation-based-code-folding-for-emacs-a-modern-replacement-for-origamiel-and-yafoldingel)
  - [Installation](#installation)
  - [Activation](#activation)
    - [Manual activation](#manual-activation)
    - [Automatic activation using hooks](#automatic-activation-using-hooks)
    - [Adjusting the shift width and default offset (unnecessary)](#adjusting-the-shift-width-and-default-offset-unnecessary)
    - [Ensuring that window-start is always visible](#ensuring-that-window-start-is-always-visible)
  - [Usage](#usage)
    - [How to check if it is working?](#how-to-check-if-it-is-working)
    - [Functions specific to outline-indent-minor-mode](#functions-specific-to-outline-indent-minor-mode)
    - [Collapsing Sections Above a Specified Outline Level](#collapsing-sections-above-a-specified-outline-level)
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
  - [Automatically Folding All Folds on Mode Activation](#automatically-folding-all-folds-on-mode-activation)
    - [How to Prevent Emacs from Searching Folded Sections](#how-to-prevent-emacs-from-searching-folded-sections)
    - [Why not use origami.el or yafolding?](#why-not-use-origamiel-or-yafolding)
    - [Why not use folding.el?](#why-not-use-foldingel)
    - [How to make Emacs indent new lines based on previous non-blank line?](#how-to-make-emacs-indent-new-lines-based-on-previous-non-blank-line)
    - [What other packages can be used to maintain proper indentation in indentation-sensitive programming languages?](#what-other-packages-can-be-used-to-maintain-proper-indentation-in-indentation-sensitive-programming-languages)
      - [Displaying vertical indentation guide bars](#displaying-vertical-indentation-guide-bars)
      - [Detecting indentation](#detecting-indentation)
    - [How to view different outline-indent folds in separate windows?](#how-to-view-different-outline-indent-folds-in-separate-windows)
  - [Comments from users](#comments-from-users)
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
  (outline-indent-ellipsis " ▼"))
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
### Adjusting the shift width and default offset (unnecessary)

**IMPORTANT: By default, manual adjustment of the shift width is unnecessary. The basic offset is automatically determined by `outline-indent`.**

If you wish to customize the indentation, you can modify `outline-indent-default-offset` and `outline-indent-shift-width` to suit your preferences:
* **`outline-indent-default-offset`**: Specifies the base indentation for each outline level. This determines how much each successive outline level is indented, shaping the visual structure of the outline.
* **`outline-indent-shift-width`**: Specifies the number of spaces to adjust the indentation when promoting or demoting a block using `(outline-indent-shift-right)` or `(outline-indent-shift-left)`.

---

It is concise, consistent, and maintains a formal tone.

### Ensuring that window-start is always visible

In some cases, Emacs may incorrectly consider a heading to be empty and scroll past it, even though it contains hidden or folded content such as child entries or overlays. This can result in a misleading view where the heading appears to be without content, despite actually containing structured data.

To mitigate this issue, it is advisable to set `make-window-start-visible` to `t`, which ensures that the beginning of the window is always visible:
```emacs-lisp
;; Ensure that the beginning of the window is always visible
(add-hook 'outline-minor-mode-hook
          #'(lambda()
              (setq-local make-window-start-visible t)))
```

## Usage

### How to check if it is working?

Run the following function to fold all indented blocks:

``` emacs-lisp
(outline-indent-close-folds)
```

### Functions specific to outline-indent-minor-mode

### Collapsing Sections Above a Specified Outline Level

Emacs allows collapsing all sections above a given outline level. For instance:

```elisp
(outline-indent-close-level 2)
```

To apply this behavior automatically whenever `outline-indent-minor-mode` is activated:

```elisp
(add-hook 'outline-indent-minor-mode-hook
          (lambda ()
            (outline-indent-close-level 2)))
```

This ensures that sections exceeding the specified level are initially collapsed.

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

## Automatically Folding All Folds on Mode Activation

The `outline-indent-minor-mode` mode can be configured to automatically collapse all foldable sections upon activation. This behavior may be applied selectively to specific modes (e.g., Python or YAML), or globally across all modes.

The following example ensures that all foldable sections are collapsed only when entering Python or YAML buffers:
```elisp
(add-hook 'outline-indent-minor-mode-hook
          #'(lambda()
              (when (or
                     ;; Python
                     (derived-mode-p 'python-mode)
                     (derived-mode-p 'python-ts-mode)
                     ;; Yaml
                     (derived-mode-p 'yaml-ts-mode)
                     (derived-mode-p 'yaml-mode))
                (outline-indent-close-folds))))
```

To collapse all foldable sections whenever `outline-indent-minor-mode` is enabled, regardless of the major mode:
```elisp
(add-hook 'outline-indent-minor-mode-hook
          #'(lambda()
              (outline-indent-close-folds)))
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

To create an indirect buffer of the current buffer, you can use `M-x clone-indirect-buffer-other-window` or the following function:

``` emacs-lisp
(clone-indirect-buffer nil t)
```

## Comments from users

- [Brandon Schneider (skarekrow)](https://github.com/jamescherti/outline-indent.el/issues/12): Thanks again for all the great work!

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
