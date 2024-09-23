# outline-indent.el - Emacs Package to Outline and Fold Text Using Indentation Levels
[![MELPA](https://melpa.org/packages/outline-indent-badge.svg)](https://melpa.org/#/outline-indent)
![Build Status](https://github.com/jamescherti/outline-indent.el/actions/workflows/ci.yml/badge.svg)
![License](https://img.shields.io/github/license/jamescherti/outline-indent.el)
![](https://raw.githubusercontent.com/jamescherti/outline-indent.el/main/.images/made-for-gnu-emacs.svg)

The `outline-indent.el` Emacs package provides a minor mode that enables code folding and outlining based on indentation levels for various indentation-based text files, such as YAML, Python, and any other indented text files.

In addition to code folding, `outline-indent.el` allows moving indented subtrees up and down, promoting and demoting sections to adjust indentation levels, customizing the ellipsis, and inserting a new line with the same indentation level as the current line, among other features.

The `outline-indent.el` package utilizes the built-in *outline-minor-mode*, which is *maintained by the Emacs developers* and is less likely to be abandoned like *origami.el* or *yafolding.el*. Since `outline-indent.el` is based on *outline-minor-mode*, it's also much much faster than *origami.el* and *yafolding.el*.

![](https://raw.githubusercontent.com/jamescherti/outline-indent.el/main/.screenshot.png)

![](https://raw.githubusercontent.com/jamescherti/outline-indent.el/main/.screenshot2.png)

The `outline-indent.el` Emacs package offers a similar functionality to Vim's `set foldmethod=indent` setting. Just as in Vim, it allows to fold and unfold code sections based on their indentation levels.

## Table of Contents
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [outline-indent.el - Emacs Package to Outline and Fold Text Using Indentation Levels](#outline-indentel---emacs-package-to-outline-and-fold-text-using-indentation-levels)
    - [Table of Contents](#table-of-contents)
    - [Installation](#installation)
    - [Activation](#activation)
        - [Manual activation](#manual-activation)
        - [Auto activation](#auto-activation)
        - [Adjusting the shift width](#adjusting-the-shift-width)
    - [Usage](#usage)
        - [Functions specific to outline-indent-minor-mode](#functions-specific-to-outline-indent-minor-mode)
            - [outline-indent-promote and outline-indent-demote](#outline-indent-promote-and-outline-indent-demote)
            - [outline-indent-move-subtree-up and outline-indent-move-subtree-down](#outline-indent-move-subtree-up-and-outline-indent-move-subtree-down)
            - [outline-indent-insert-heading](#outline-indent-insert-heading)
        - [Vanilla Emacs](#vanilla-emacs)
        - [Evil mode](#evil-mode)
    - [Frequently asked questions](#frequently-asked-questions)
        - [Maintaining blank lines between folded sections](#maintaining-blank-lines-between-folded-sections)
        - [Why not use origami.el or yafolding?](#why-not-use-origamiel-or-yafolding)
        - [Why not use folding.el?](#why-not-use-foldingel)
    - [License](#license)
    - [Links](#links)

<!-- markdown-toc end -->

## Installation

To install the `outline-indent` from MELPA:

1. If you haven't already done so, [add MELPA repository to your Emacs configuration](https://melpa.org/#/getting-started).

2. Add the following code to your Emacs init file to install `outline-indent` from MELPA:
``` emacs-lisp
(use-package outline-indent
  :ensure t
  :custom
  (outline-indent-ellipsis " ▼ "))
```

## Activation

### Manual activation

Once installed, the minor mode can be activated using:
``` emacs-lisp
(outline-indent-minor-mode)
```

### Auto activation

The minor mode can also be automatically activated for a certain mode. For example for Python and YAML:
``` emacs-lisp
;; Python
(add-hook 'python-mode-hook #'outline-indent-minor-mode)
(add-hook 'python-ts-mode-hook #'outline-indent-minor-mode)

;; YAML
(add-hook 'yaml-mode-hook #'outline-indent-minor-mode)
(add-hook 'yaml-ts-mode-hook #'outline-indent-minor-mode)
```

### Adjusting the shift width

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
   - **`outline-indent-shift-width`**: This variable determines the number of spaces by which to adjust the indentation when promoting or demoting an indented block with `(outline-promote)` and `(outline-demote)`.

2. **Why Customize These Values?**:
   - **Language-specific Indentation**: Different programming languages and file formats have different indentation standards. Python typically uses 4 spaces per indentation level, while YAML often uses 2 spaces. Customizing these values for different modes ensures that your outline structure is consistent with the language's indentation practices.
   - **Promotion and Demotion**: When you use `outline-promote` and `outline-demote` functions, these settings control how much the outline level is adjusted. For instance, in Python mode, promoting a block of code (moving it to a higher outline level) will decrease its indentation by 4 spaces, and demoting it will increase its indentation by 4 spaces.

3. **Default Behavior**:
   - By default, `outline-indent-default-offset` is set to 1, which works with any indentation level, as even a single space is enough to fold any indented block using `outline-indent`.
   - By default, `outline-indent-default-shift-width` is `nil`, which means it inherits the value of `outline-indent-default-offset`. If you do not explicitly set `outline-indent-shift-width`, the promote and demote operations will use the same value as the offset. This default behavior works well in many cases, but fine-tuning these values can be necessary for languages or formats with specific indentation needs.

## Usage

### Functions specific to outline-indent-minor-mode

#### outline-indent-promote and outline-indent-demote

*(By default, `outline-indent-advise-outline-functions` is set to t, which means that you can also use the built-in outline functions `(outline-promote)` and `(outline-demote)` as an alternative to `(outline-indent-promote)` and `(outline-indent-demote)`)*

These functions can be used to decrease and increase the indentation level of indented blocks.

To increase indentation:
``` emacs-lisp
(outline-indent-demote)
```

To decrease indentation:
``` emacs-lisp
(outline-indent-promote)
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
- `outline-hide-body`: Hide all body lines in buffer, leaving all headings visible.
- `outline-hide-other`: Hide everything except current body and parent and top-level headings.
- `outline-hide-entry`: Hide the body directly following this heading.
- `outline-hide-leaves`: Hide the body after this heading and at deeper levels.
- `outline-hide-subtree`: Hide everything after this heading at deeper levels.
- `outline-show-children`: Show all direct subheadings of this heading.
- `outline-hide-sublevels`: Hide everything but the top LEVELS levels of headers, in whole buffer.
- `outline-show-all`: Show all of the text in the buffer.
- `outline-show-entry`: Show the body directly following this heading.
- `outline-show-subtree`: Show everything after this heading at deeper levels.
- `outline-show-branches`: Show all subheadings of this heading, but not their bodies.
- `outline-show-children`: Show all direct subheadings of this heading.

You can also indent/unindent and move subtree up and down using:

- `(outline-indent-demote)` and `(outline-indent-promote)`: Indent or unindent the entire subtree.
- `(outline-indent-move-subtree-down)` and `(outline-indent-move-subtree-up)` to move the current subtree up or down.
- `(outline-insert-heading)` to insert a new line with the same indentation level/depth as the current line just before the next heading that shares the same or less indentation level.

Move to the next and previous visible fold:
- `outline-previous-visible-heading`
- `outline-next-visible-heading`

Move forward or backward to the same indentation level:
- `outline-forward-same-level`: Move forward to the same indentation level as the one under the cursor.
- `outline-backward-same-level`: Move backward to the the same indentation level as as the one under the cursor.

### Evil mode

In Evil mode, `outline-indent` works out of the box if you install `evil-collection`, and you can use the Evil and evil-collection keyboard mappings:
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
    (evil-define-key 'normal 'local (kbd "M-h") #'outline-indent-promote)
    (evil-define-key 'normal 'local (kbd "M-l") #'outline-indent-demote)

    ;; Set `M-k` and `M-j` to move indented blocks up and down
    (evil-define-key 'normal 'local (kbd "M-k") #'outline-indent-move-subtree-up)
    (evil-define-key 'normal 'local (kbd "M-j") #'outline-indent-move-subtree-down)

    ;; Set C-<return> to insert a new line with the same indentation
    ;; level/depth as the current line just before the next heading
    (evil-define-key '(normal insert) 'local (kbd "C-<return>")
      (defun my-evil-outline-indent-insert-heading ()
        (interactive)
        (outline-indent-insert-heading)
        (evil-insert-state))))

  (add-hook 'outline-indent-minor-mode-hook #'my-evil-define-key-outline-indent-minor-mode))
```

## Frequently asked questions

### Maintaining blank lines between folded sections

The `outline-blank-line` variable can be set to `t` (true) to maintain blank lines between folded sections, making it easier to distinguish between folds:

``` emacs-lisp
(setq outline-blank-line t)
```

### Why not use origami.el or yafolding?

The `origami.el` and `yafolding.el` package are not reliable method for folding indented code because they are:
- No longer maintained (abandoned),
- Slow,
- Known to have bugs that affect their reliability and performance.

On the other hand, `outline-indent.el` leverages the built-in `outline-minor-mode`, which is:
- Fast,
- Actively maintained by the Emacs developers.

### Why not use folding.el?

The `folding.el` package is no longer maintained (abandoned) and uses markers in the buffer to annotate folds. It does not support utilizing indentation levels to determine foldable sections.

In contrast, `outline-indent.el` uses indentation levels to determine foldable sections.

## License

Copyright (C) 2024 [James Cherti](https://www.jamescherti.com)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.

## Links

- [outline-indent.el @GitHub](https://github.com/jamescherti/outline-indent.el)
- [outline-indent.el @MELPA](https://melpa.org/#/outline-indent)
- Article: [outline-indent.el – Indentation based Folding and Outlining in Emacs](https://www.jamescherti.com/fold-outline-indentation-emacs-package/)

Other Emacs packages by the same author:
- [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d): This repository hosts a minimal Emacs configuration designed to serve as a foundation for your vanilla Emacs setup and provide a solid base for an enhanced Emacs experience.
- [vim-tab-bar.el](https://github.com/jamescherti/vim-tab-bar.el): Make the Emacs tab-bar Look Like Vim’s Tab Bar.
- [easysession.el](https://github.com/jamescherti/easysession.el): Easysession is lightweight Emacs session manager that can persist and restore file editing buffers, indirect buffers/clones, Dired buffers, the tab-bar, and the Emacs frames (with or without the Emacs frames size, width, and height).
- [elispcomp](https://github.com/jamescherti/elispcomp): A command line tool that allows compiling Elisp code directly from the terminal or from a shell script. It facilitates the generation of optimized .elc (byte-compiled) and .eln (native-compiled) files.
- [tomorrow-night-deepblue-theme.el](https://github.com/jamescherti/tomorrow-night-deepblue-theme.el): The Tomorrow Night Deepblue Emacs theme is a beautiful deep blue variant of the Tomorrow Night theme, which is renowned for its elegant color palette that is pleasing to the eyes. It features a deep blue background color that creates a calming atmosphere. The theme is also a great choice for those who miss the blue themes that were trendy a few years ago.
- [Ultyas](https://github.com/jamescherti/ultyas/): A command-line tool designed to simplify the process of converting code snippets from UltiSnips to YASnippet format.
- [dir-config.el](https://github.com/jamescherti/dir-config.el): Automatically find and evaluate .dir-config.el Elisp files to configure directory-specific settings.
- [flymake-bashate.el](https://github.com/jamescherti/flymake-bashate.el): A package that provides a Flymake backend for the bashate Bash script style checker.
- [flymake-ansible-lint.el](https://github.com/jamescherti/flymake-ansible-lint.el): An Emacs package that offers a Flymake backend for `ansible-lint`.
