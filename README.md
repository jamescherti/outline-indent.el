# outline-indent.el - Emacs Package to Outline and Fold Text Using Indentation Levels

The `outline-indent.el` Emacs package provides a minor mode for Emacs that enables **code folding and outlining based on indentation levels**.

The `outline-indent.el` leverages the built-in outline-minor-mode to automatically detect and fold sections according to their indentation hierarchy. The built-in outline-minor-mode enables utilizing functions such as demote, move indented blocks up, and move indented blocks down, significantly enhancing the editing experience for indented text. These functions are invaluable for restructuring and reorganizing content within indented sections.

![](https://raw.githubusercontent.com/jamescherti/outline-indent.el/main/.screenshot.png)

![](https://raw.githubusercontent.com/jamescherti/outline-yaml.el/main/.screenshot.png)

The `outline-indent.el` Emacs package offers a similar functionality to Vim's `set foldmethod=indent` setting. Just as in Vim, it allows to fold and unfold code sections based on their indentation levels.

## Installation

### Install using straight

To install the `outline-indent` using `straight.el`:

1. If you haven't already done so, [add the straight.el bootstrap code](https://github.com/radian-software/straight.el?tab=readme-ov-file#getting-started) to your init file.

2. Add the following code to your Emacs init file:
```
(use-package outline-indent
  :ensure t
  :straight (outline-indent
             :type git
             :host github
             :repo "jamescherti/outline-indent.el")
  :custom
  (outline-indent-ellipsis " ▼ "))
```

## Activation

Once installed, the minor mode can be activated using:
```
(outline-indent-minor-mode)
```

The minor mode can also be automatically activated for a certain mode. For example for text-mode:
```
(add-hook 'text-mode-hook #'outline-indent-minor-mode)
```

## Usage

### Functions specific to outline-indent-minor-mode

#### outline-indent-insert-heading

The `(outline-indent-insert-heading)` function inserts a new line with the same indentation level/depth as the current line just before the next heading that shares the same or less indentation level. It finds the nearest non-empty line with the same or less indentation as the current line and inserts a new line before it.

In `outline-indent-minor-mode`, where most lines are treated as headings, this function is suitable for maintaining consistent indentation within the outline structure. It can be used as an alternative to `outline-insert-heading` to insert content at the same indentation level after the current fold.

Example usage:
```
(outline-indent-insert-heading)
```

By default, this function can be invoked by pressing `C-<return>` in buffers where `outline-indent-minor-mode` is active.

If you are an Emacs Evil user, you may want to make `C-<return>` call the function above and switch to insert mode:

``` emacs-lisp
(evil-define-key '(normal insert) outline-indent-minor-mode-map
  (kbd "C-<return>")
  (defun my-evil-outline-indent-insert-heading ()
    (interactive)
    (outline-indent-insert-heading)
    (evil-insert-state)))
```

### Vanilla Emacs

Use the standard `outline-mode`/`outline-minor-mode` commands to fold and unfold sections of your YAML file:
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

### Evil mode

In Evil mode, `outline-indent` works out of the box, and you can use the Evil keyboard mappings: zo, zc, zO, zC, za, zr, and zm to manage folds.

## Frequently asked questions

### Maintaining blank lines between folded sections

The `outline-blank-line` variable can be set to `t` (true) to maintain blank lines between folded sections, making it easier to distinguish between folds:

```
(setq outline-blank-line t)
```

I recommend you read [Enhancing up and down subtree movement in outline-mode and outline-minor-mode](https://www.jamescherti.com/outline-mode-enhance-move-subtree-up-down/) to make `outline-move-subtree-up` and `outline-move-subtree-down` ignore the `outline-blank-line` variable, which allows moving the whole block up and down, including the blank line.

### Why not use origami.el?

The `origami.el` package is no longer actively maintained and has known bugs that can affect its reliability and performance.

On the other hand, `outline-indent.el` leverages the built-in `outline-minor-mode`, which is actively maintained by the Emacs developers.

## License

Copyright (C) 2024 [James Cherti](https://www.jamescherti.com)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.

## Links

- [outline-indent.el @GitHub](https://github.com/jamescherti/outline-indent.el)
- Similar package from the same author: [outline-yaml.el @GitHub](https://github.com/jamescherti/outline-yaml.el)
