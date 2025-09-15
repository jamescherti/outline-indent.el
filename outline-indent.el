;;; outline-indent.el --- Folding text based on indentation (origami alternative) -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1.1.2
;; URL: https://github.com/jamescherti/outline-indent.el
;; Keywords: outlines
;; Package-Requires: ((emacs "26.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; The outline-indent.el Emacs package provides a minor mode that enables code
;; folding based on indentation levels.
;;
;; The outline-indent.el package is a fast and reliable alternative to the
;; origami.el and yafolding.el packages. (origami.el and yafolding.el are no
;; longer maintained, slow, and known to have bugs that impact their reliability
;; and performance.)
;;
;; In addition to code folding, outline-indent allows:
;; - Moving indented subtrees up and down,
;; - indent/unindent sections to adjust indentation levels,
;; - customizing the ellipsis,
;; - inserting a new line with the same indentation level as the current line,
;; - and other features.
;;
;; The outline-indent.el package uses the built-in outline-minor-mode, which is
;; maintained by the Emacs developers and is less likely to be abandoned like
;; origami.el or yafolding.el. Since outline-indent.el is based on
;; outline-minor-mode, it's also much much faster than origami.el and
;; yafolding.el.
;;
;; Installation from MELPA:
;; ------------------------
;; (use-package outline-indent
;;   :ensure t
;;   :custom
;;   (outline-indent-ellipsis " ▼ "))
;;
;; Activation:
;; -----------
;; Once installed, the minor mode can be activated using:
;;   (outline-indent-minor-mode)
;;
;; Activation using a hook:
;; ------------------------
;; The minor mode can also be automatically activated for a certain mode. For
;; example for Python and YAML:
;;   ;; Python
;;   (add-hook 'python-mode-hook #'outline-indent-minor-mode)
;;   (add-hook 'python-ts-mode-hook #'outline-indent-minor-mode)
;;
;;   ;; YAML
;;   (add-hook 'yaml-mode-hook #'outline-indent-minor-mode)
;;   (add-hook 'yaml-ts-mode-hook #'outline-indent-minor-mode)
;;
;; Links:
;; ------
;; - More information about outline-indent (Frequently asked questions, usage...):
;;   https://github.com/jamescherti/outline-indent.el

;;; Code:

(require 'outline)

;;; Customizations

(defgroup outline-indent nil
  "Folding text based on indentation."
  :group 'outline-indent
  :prefix "outline-indent-")

(defcustom outline-indent-default-offset nil
  "Default indentation offset.
If nil, the offset is automatically determined based on the current mode.
This value is used to calculate outline levels from the current indentation."
  :type '(choice (integer :tag "Custom width")
                 (const :tag "Auto detect" nil))
  :group 'outline-indent)

(defcustom outline-indent-shift-width nil
  "Shift width used for indentation adjustments during promotion and demotion.
If nil, this value defaults to `outline-indent-default-offset', which is
automatically determined according to the current mode."
  :type '(choice (integer :tag "Custom width")
                 (const :tag "Auto detect" nil))
  :group 'outline-indent)

(defcustom outline-indent-ellipsis nil
  "String used as the ellipsis character in `outline-indent-mode'.

When non-nil, this string will be used as the display text for outline folding
ellipses, replacing the default `outline-ellipsis` (which is typically \"...\"
or a Unicode character such as \"…\" depending on your configuration).

This setting applies only to buffers where `outline-indent-minor-mode' is
enabled. When enabled, the minor mode installs a buffer-local display
table that replaces the default ellipsis with this string. If this
variable is set to nil, no replacement occurs and the default ellipsis
mechanism remains in place.

Note: Trailing whitespace after the ellipsis is automatically removed to prevent
a specific visual and editing issue. When lines are wrapped or truncated, any
trailing space following the ellipsis may appear on a separate visual line,
creating the illusion of an additional line. This can mislead the user and cause
confusion during editing—for example, deleting what appears to be only the
whitespace may in fact delete the entire folded line.

Recommended values include \" ▼\", \"↴\", \"…\", \"...\", or any compact string
suitable for representing folded content."
  :type '(choice string (const nil))
  :group 'outline-indent)


(define-obsolete-variable-alias
  'outline-indent-make-window-start-visible
  'make-window-start-visible
  "1.1.2"
  "Obsolete. Use `make-window-start-visible' instead.")

(defcustom outline-indent-insert-heading-add-blank-line nil
  "Non-nil to make `outline-indent-insert-heading' add a blank line.

If non-nil, a blank line is inserted immediately before the newly added line,
and the cursor is moved to it. This behavior is useful for maintaining a visual
separation between the new indented block and surrounding content."
  :type 'boolean
  :group 'outline-indent)

(defun outline-indent--advise-func (advise)
  "Advise `outline' functions.
When ADVISE is set to t, advise the `outline' functions."
  (if advise
      ;; Advise the built-in `outline-mode' and `outline-minor-mode'
      ;; functions to improve compatibility with
      ;; `outline-indent-minor-mode'. The built-in `outline-minor-mode'
      ;; functions will work exactly as before and will only exhibit
      ;; different behavior when `outline-indent-minor-mode' is active.
      (progn
        (advice-add 'outline-promote :around
                    #'outline-indent--advice-promote)
        (advice-add 'show-entry :around
                    #'outline-indent--advice-show-entry)
        (advice-add 'hide-subtree :around
                    #'outline-indent--advice-outline-hide-subtree)
        (advice-add 'outline-hide-subtree :around
                    #'outline-indent--advice-outline-hide-subtree)
        (advice-add 'outline-show-entry :around
                    #'outline-indent--advice-show-entry)
        (advice-add 'outline-demote :around
                    #'outline-indent--advice-demote)
        (advice-add 'outline-insert-heading :around
                    #'outline-indent--advice-insert-heading)
        (advice-add 'outline-forward-same-level :around
                    #'outline-indent--advice-forward-same-level)
        (advice-add 'outline-backward-same-level :around
                    #'outline-indent--advice-backward-same-level)
        (advice-add 'outline-move-subtree-up :around
                    #'outline-indent--advice-move-subtree-up)
        (advice-add 'outline-move-subtree-down :around
                    #'outline-indent--advice-move-subtree-down))
    ;; Disable
    (advice-remove 'outline-promote
                   #'outline-indent--advice-promote)
    (advice-remove 'show-entry
                   #'outline-indent--advice-show-entry)
    (advice-remove 'hide-subtree
                   #'outline-indent--advice-outline-hide-subtree)
    (advice-remove 'outline-hide-subtree
                   #'outline-indent--advice-outline-hide-subtree)
    (advice-remove 'outline-show-entry
                   #'outline-indent--advice-show-entry)
    (advice-remove 'outline-demote
                   #'outline-indent--advice-demote)
    (advice-remove 'outline-insert-heading
                   #'outline-indent--advice-insert-heading)
    (advice-remove 'outline-forward-same-level
                   #'outline-indent--advice-forward-same-level)
    (advice-remove 'outline-backward-same-level
                   #'outline-indent--advice-backward-same-level)
    (advice-remove 'outline-move-subtree-up
                   #'outline-indent--advice-move-subtree-up)
    (advice-remove 'outline-move-subtree-down
                   #'outline-indent--advice-move-subtree-down)))

(defcustom outline-indent-advise-outline-functions t
  "If non-nil, advises built-in `outline' functions to improve compatibility.

When set to t, advises built-in `outline-minor-mode' functions to enhance
compatibility with `outline-indent-minor-mode'. When set to nil, removes
the advice.

Functions that will be advised include:
- `outline-promote'
- `outline-demote'
- `outline-insert-heading'
- `outline-forward-same-level'
- `outline-backward-same-level'
- `outline-move-subtree-up'
- `outline-move-subtree-down'

It is recommended to keep this set to t for improved behavior."
  :type 'boolean
  :set
  (lambda (symbol value)
    (set-default symbol value)
    (outline-indent--advise-func value))
  :group 'outline-indent)

(defvar outline-indent-minor-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `outline-indent-minor-mode'.")

;;; Internal variables

(defconst outline-indent--has-ts-modes
  (and (boundp 'emacs-version)
       (version<= "29.1" emacs-version))
  "Non-nil if Emacs supports tree-sitter modes (Emacs 29.1+).")

(defvar outline-indent--mode-basic-offset-map
  '((typescript-ts-base-mode typescript-ts-mode-indent-offset)
    (ada-ts-mode     ada-ts-mode-indent-offset)
    (sh-mode         sh-basic-offset)
    (c++-mode        c-basic-offset)
    (c-mode          c-basic-offset)
    (cmake-mode      cmake-tab-width)
    (cperl-mode      cperl-indent-level)
    (crystal-mode    crystal-indent-level)
    (css-mode        css-indent-offset)
    (d-mode          c-basic-offset)
    (default         standard-indent)
    (enh-ruby-mode   enh-ruby-indent-level)
    (erlang-mode     erlang-indent-level)
    (java-mode       c-basic-offset)
    (jde-mode        c-basic-offset)
    (js-mode         js-indent-level)
    (js2-mode        js2-basic-offset)
    (js3-mode        js3-indent-level)
    (json-mode       js-indent-level)
    (lua-mode        lua-indent-level)
    (nxml-mode       nxml-child-indent)
    (objc-mode       c-basic-offset)
    (pascal-mode     pascal-indent-level)
    (perl-mode       perl-indent-level)
    (php-mode        c-basic-offset)
    (plantuml-mode   plantuml-indent-level)
    (protobuf-mode   c-basic-offset)
    (pug-mode        pug-tab-width)
    (raku-mode       raku-indent-offset)
    (ruby-mode       ruby-indent-level)
    (rust-mode       rust-indent-offset)
    (rustic-mode     rustic-indent-offset)
    (scala-mode      scala-indent:step)
    (sgml-mode       sgml-basic-offset)
    (swift-mode      swift-mode:basic-offset)
    (typescript-mode typescript-indent-level)
    (ada-mode        ada-indent)
    (bash-ts-mode    sh-basic-offset)
    (js-json-mode    js-indent-level)
    (c++-ts-mode     c-ts-mode-indent-offset)
    (c-ts-mode       c-ts-mode-indent-offset)
    (cmake-ts-mode   cmake-ts-mode-indent-offset)
    (go-ts-mode      go-ts-mode-indent-offset)
    (gpr-ts-mode     gpr-ts-mode-indent-offset)
    (java-ts-mode    java-ts-mode-indent-offset)
    (js-ts-mode      js-indent-level)
    (json-ts-mode    json-ts-mode-indent-offset)
    (rust-ts-mode    rust-ts-mode-indent-offset)
    (yaml-ts-mode    yaml-indent-offset 2)
    (python-mode     python-indent-offset)
    (python-ts-mode  python-indent-offset)
    (ursa-ts-mode    ursa-ts-mode-indent-offset)
    (vhdl-mode       vhdl-basic-offset)
    (xquery-mode     xquery-mode-indent-width)
    (groovy-mode     groovy-indent-offset tab-width)
    (yaml-mode       yaml-indent-offset tab-width)
    (web-mode        web-mode-markup-indent-offset
                     web-mode-code-indent-offset
                     web-mode-sql-indent-offset
                     web-mode-css-indent-offset))
  "A mapping from hook variables to language types.")

;;; Internal Functions

(defun outline-indent--get-mode-basic-offset-entry (mode)
  "Return the mapping entry for MODE or its derived-mode-parent.
Searches `outline-indent--mode-basic-offset-map', falling back to `default'."
  (when mode
    (or (assoc mode outline-indent--mode-basic-offset-map)
        (outline-indent--get-mode-basic-offset-entry
         (get mode 'derived-mode-parent))
        (assoc 'default outline-indent--mode-basic-offset-map))))

(defun outline-indent--get-mode-basic-offset (mode)
  "Return the basic indentation offset for MODE.
If the mapping entry contains multiple variables, the last bound
symbol is used. Returns `standard-indent` if no suitable variable
is found."
  (when-let* ((entry (outline-indent--get-mode-basic-offset-entry mode)))
    (let ((list-vars (cdr entry))
          result)
      (catch 'done
        (dolist (var list-vars)
          (cond
           ((numberp var)
            (setq result var)
            (throw 'done t))

           ((and (symbolp var)
                 (boundp var))
            (setq result (symbol-value var))
            (throw 'done t)))))

      (or result
          1))))

(defun outline-indent--setup-basic-offset ()
  "Initialize `outline-indent-default-offset' and `outline-indent-shift-width'.

If either variable is not already set, determine the default value for the
current major-mode using `outline-indent--get-mode-basic-offset', and set the
variables locally. This ensures that outline levels and indentation shifts
follow the mode-specific coding style automatically."
  (unless (or outline-indent-default-offset
              outline-indent-shift-width)
    (let ((major-mode-offset (outline-indent--get-mode-basic-offset
                              major-mode)))
      (unless outline-indent-default-offset
        (setq-local outline-indent-default-offset major-mode-offset))

      (unless outline-indent-shift-width
        (setq-local outline-indent-shift-width major-mode-offset)))))

(defun outline-indent-level ()
  "Determine the outline level based on the current indentation."
  (+ 1 (/ (current-indentation)
          (max (if outline-indent-default-offset
                   outline-indent-default-offset
                 1)
               1))))

(defun outline-indent--update-ellipsis ()
  "Update the buffer's outline ellipsis."
  (when outline-indent-ellipsis
    (let* ((display-table (or buffer-display-table (make-display-table)))
           (face-offset (* (face-id 'shadow) (ash 1 22)))
           (value (vconcat (mapcar (lambda (c) (+ face-offset c))
                                   ;; Trim trailing whitespace after the
                                   ;; ellipsis, as it can be misleading when the
                                   ;; line is not truncated. Wrapping may
                                   ;; display only the space after the ellipsis
                                   ;; on the next line, creating the illusion of
                                   ;; a new line. Deleting that apparent "new
                                   ;; line" may delete the entire logical line
                                   ;; containing the ellipsis.
                                   (string-trim-right outline-indent-ellipsis)))))
      (set-display-table-slot display-table 'selective-display value)
      (setq buffer-display-table display-table))))

(defun outline-indent--next-lower-or-equal-indentation ()
  "Go to the same indentation level/depth as the current line.
Go to just before the next heading that shares the same or less indentation
level.

This function finds the nearest non-empty line with the same or less
indentation as the current line."
  (let ((initial-indentation nil)
        (found-point nil))
    (save-excursion
      (beginning-of-visual-line)
      (setq initial-indentation (outline-indent-level))
      (while (and (not found-point) (not (eobp)))
        (forward-line 1)
        (if (and (>= initial-indentation (outline-indent-level))
                 (not (looking-at-p "^[ \t]*$")))
            (setq found-point (point))))

      (when (and (not found-point) (eobp))
        (setq found-point (point)))

      (when found-point
        (goto-char found-point)
        (forward-line -1)
        (end-of-line)
        (point)))))

(defun outline-indent-insert-heading ()
  "Insert a new line with the same indentation level/depth as the current line.
The line is inserted just before the next heading that shares the same or less
indentation level.

This function finds the nearest non-empty line with the same or less
indentation as the current line and inserts a new line before it.

In `outline-indent-minor-mode', where most lines are treated as headings,
this function is suitable for maintaining consistent indentation within the
outline structure. It can be used as an alternative to `outline-insert-heading'
to insert content at the same indentation level after the current fold."
  (interactive)
  (let ((initial-indentation (save-excursion (beginning-of-visual-line)
                                             (current-indentation)))
        (point (outline-indent--next-lower-or-equal-indentation)))
    (when point
      (goto-char point)
      (newline)
      (when outline-indent-insert-heading-add-blank-line
        (newline)
        (forward-line -1))
      (indent-to initial-indentation))))

(defun outline-indent-move-subtree-up (&optional arg)
  "Move the current subtree up past ARG headlines of the same level.
This function ensures the last blank line is included, even when
`outline-blank-line' is set to t. It also restores the cursor position,
addressing the issue where the cursor might be reset after the operation."
  (interactive "p")
  (unless arg
    (setq arg 1))
  (outline-indent-move-subtree-down (- arg)))

(defun outline-indent--deactivate-region ()
  "Deactivate the current region and move point to the start of the region."
  (when (use-region-p)
    (goto-char (region-beginning))
    (deactivate-mark)))

(defun outline-indent-shift-right (&optional _which arg)
  "Increasing the indentation level.
The global variable `outline-indent-shift-width' or
`outline-indent-default-offset' is used to determine the number of spaces to
indent the subtree.
WHICH is ignored (backward compatibility with `outline-demote').
If ARG is positive, indent the outline. If ARG is negative, unindent the
outline. Defaults to 1 if ARG is nil."
  (interactive)
  (unless arg
    (setq arg 1))
  (outline-indent--deactivate-region)
  (let ((shift-right (>= arg 0))
        (column (current-column))
        (shift-width
         (cond (outline-indent-shift-width
                (max outline-indent-shift-width 1))

               (t
                (max outline-indent-default-offset 1)))))
    (let ((folded (save-match-data
                    (outline-end-of-heading)
                    (outline-invisible-p))))
      (save-excursion
        (outline-back-to-heading)
        (let ((start (point))
              (end (save-excursion
                     (outline-end-of-subtree)
                     (point))))
          (indent-rigidly start end (if shift-right
                                        shift-width
                                      (* -1 shift-width)))))
      (if shift-right
          (move-to-column (+ column shift-width))
        (move-to-column (max (- column shift-width) 0)))
      (if folded
          (outline-hide-subtree)))))

(defun outline-indent-shift-left (&optional _which)
  "Decreasing the indentation level. Equivalent to `outline-promote'.
The global variable `outline-indent-shift-width' or
`outline-indent-default-offset' is used to determine the number of spaces to
unindent the subtree.
WHICH is ignored (backward compatibility with `outline-promote')."
  (interactive)
  (outline-indent-shift-right nil -1))

(defalias 'outline-indent-demote #'outline-indent-shift-right
  "Deprecated alias for `outline-indent-shift-right'.")
(make-obsolete 'outline-indent-demote 'outline-indent-shift-right "1.1.1")

(defalias 'outline-indent-promote #'outline-indent-shift-left
  "Deprecated alias for `outline-indent-shift-left'.")
(make-obsolete 'outline-indent-promote 'outline-indent-shift-left "1.1.1")

(defun outline-indent--advice-promote (orig-fun &rest args)
  "Advice function for `outline-indent-shift-left'.
If `outline-indent-minor-mode' is active, use `outline-indent-insert-heading'.
Otherwise, call the original function with the given arguments.
ORIG-FUN is the original function being advised, and ARGS are its arguments."
  (if (bound-and-true-p outline-indent-minor-mode)
      (outline-indent-shift-left)
    (apply orig-fun args)))

(defun outline-indent--advice-show-entry (orig-fun &rest args)
  "Advice function for `outline-show-entry'.
If `outline-indent-minor-mode' is active, use `outline-indent-open-fold'.
Otherwise, call the original function with the given arguments.
ORIG-FUN is the original function being advised, and ARGS are its arguments."
  (if (bound-and-true-p outline-indent-minor-mode)
      (outline-indent-open-fold)
    (apply orig-fun args)))

(defun outline-indent--advice-outline-hide-subtree (orig-fun &rest args)
  "Advice function for `outline-hide-subtre'.
If `outline-indent-minor-mode' is active, use `outline-indent-close-fold'.
Otherwise, call the original function with the given arguments. ORIG-FUN is the
original function being advised, and ARGS are its arguments."
  (if (bound-and-true-p outline-indent-minor-mode)
      (outline-indent-close-fold)
    (apply orig-fun args)))

(defun outline-indent--advice-demote (orig-fun &rest args)
  "Advice function for `outline-indent-shift-right'.
If `outline-indent-minor-mode' is active, use `outline-indent-insert-heading'.
Otherwise, call the original function with the given arguments.
ORIG-FUN is the original function being advised, and ARGS are its arguments."
  (if (bound-and-true-p outline-indent-minor-mode)
      (outline-indent-shift-right)
    (apply orig-fun args)))

(defun outline-indent-move-subtree-down (&optional arg)
  "Move the current subtree down past ARG headlines of the same level.
This function ensures the last blank line is included, even when
`outline-blank-line' is set to t. It also restores the cursor position,
addressing the issue where the cursor might be reset after the operation."
  (interactive "p")
  (unless arg
    (setq arg 1))
  (outline-indent--deactivate-region)
  (let ((column (current-column)))
    (unwind-protect
        (progn
          (outline-back-to-heading)
          (let* ((movfunc (if (> arg 0) 'outline-get-next-sibling
                            'outline-get-last-sibling))
                 ;; Find the end of the subtree to be moved as well as the point
                 ;; to move it to, adding a newline if necessary, to ensure
                 ;; these points are at bol on the line below the subtree.
                 (add-new-line nil)
                 (end-point-func (lambda (respect-outline-blank-line)
                                   (let ((outline-blank-line
                                          (if respect-outline-blank-line
                                              outline-blank-line
                                            nil)))
                                     (outline-end-of-subtree))

                                   (cond
                                    ((eq (char-after) ?\n)
                                     (forward-char 1))

                                    ((and (eobp) (not (bolp)))
                                     (insert "\n"))

                                    ((and (< arg 0) outline-blank-line
                                          (save-excursion
                                            ;; Ensure the previous line is not
                                            ;; empty
                                            (forward-line -1)
                                            (not (string-blank-p
                                                  (string-trim
                                                   (thing-at-point 'line t)))))
                                          (eobp) (bolp))
                                     (setq add-new-line t)))

                                   (point)))
                 (beg (point))
                 (folded (save-match-data
                           (outline-end-of-heading)
                           (outline-invisible-p)))
                 (end (save-match-data
                        (funcall end-point-func nil)))
                 (ins-point (make-marker))
                 (cnt (abs arg)))
            ;; Find insertion point, with error handling.
            (goto-char beg)
            (while (> cnt 0)
              (or (funcall movfunc)
                  (progn (goto-char beg)
                         (user-error "Cannot move past superior level")))
              (setq cnt (1- cnt)))
            (if (> arg 0)
                ;; Moving forward - still need to move over subtree.
                (funcall end-point-func nil))
            (when (> arg 0)
              (when (and (eobp) (bolp)
                         (save-excursion
                           (forward-line -1)
                           (not (string-blank-p (thing-at-point 'line t)))))
                (insert "\n")))
            (move-marker ins-point (point))
            ;; Fix when moving the subtree of the node immediately preceding
            ;; the last one to the position after the last one.
            (let ((data (delete-and-extract-region beg end)))
              (insert data))
            (when add-new-line
              (insert "\n"))
            (goto-char ins-point)
            (if folded (outline-hide-subtree))
            (move-marker ins-point nil)))
      (move-to-column column))))

(defun outline-indent--advice-insert-heading (orig-fun &rest args)
  "Advice function for `outline-insert-heading'.

If `outline-indent-minor-mode' is active, use `outline-indent-insert-heading'.
Otherwise, call the original function with the given arguments.

ORIG-FUN is the original function being advised, and ARGS are its arguments."
  (if (bound-and-true-p outline-indent-minor-mode)
      ;; Use `outline-indent-insert-heading' if `outline-indent-minor-mode' is
      ;; active
      (outline-indent-insert-heading)
    ;; Call the original function with its arguments if
    ;; `outline-indent-minor-mode' is not active
    (apply orig-fun args)))

(defun outline-indent--advice-move-subtree-up (orig-fun &rest args)
  "Advice for `outline-move-subtree-up'.
It only changes the behavior when `outline-indent-minor-mode' is active.
ORIG-FUN is the original function being advised, and ARGS are its arguments."
  (if (bound-and-true-p outline-indent-minor-mode)
      ;; Adjust behavior specific to `outline-indent-minor-mode`
      (outline-indent-move-subtree-up)
    ;; Apply the original function without modification
    (apply orig-fun args)))

(defun outline-indent--advice-move-subtree-down (orig-fun &rest args)
  "Advice for `outline-move-subtree-down'.

It only changes the behavior when `outline-indent-minor-mode' is active.
ORIG-FUN is the original function being advised, and ARGS are its arguments."
  (if (bound-and-true-p outline-indent-minor-mode)
      ;; Adjust behavior specific to `outline-indent-minor-mode`
      (outline-indent-move-subtree-down)
    ;; Apply the original function without modification
    (apply orig-fun args)))

(defun outline-indent--advice-forward-same-level (orig-fun &rest args)
  "Advice for `outline-forward-same-level'.
It only changes the behavior when `outline-indent-minor-mode' is active.
ORIG-FUN is the original function being advised, and ARGS are its arguments."
  (if (bound-and-true-p outline-indent-minor-mode)
      ;; Adjust behavior specific to `outline-indent-minor-mode`
      (let ((column (current-column)))
        (unwind-protect
            (progn
              (outline-indent--deactivate-region)
              (apply orig-fun args))
          (move-to-column column)))
    ;; Apply the original function without modification
    (apply orig-fun args)))

(defun outline-indent--advice-backward-same-level (orig-fun &rest args)
  "Advice for `outline-backward-same-level'.
It only changes the behavior when `outline-indent-minor-mode' is active.
ORIG-FUN is the original function being advised, and ARGS are its arguments."
  (if (bound-and-true-p outline-indent-minor-mode)
      ;; Adjust behavior specific to `outline-indent-minor-mode`
      (let ((column (current-column)))
        (unwind-protect
            (progn
              (outline-indent--deactivate-region)
              (apply orig-fun args))
          (move-to-column column)))
    ;; Apply the original function without modification
    (apply orig-fun args)))

;;; Functions

(defun outline-indent-backward-same-level (&optional arg)
  "Move backward to the ARG'th subheading at same indentation level as this one.
Stop at the first and last indented blocks of a superior indentation."
  (interactive "p")
  (unless arg
    (setq arg 1))
  (if (advice-member-p 'outline-indent--advice-backward-same-level
                       'outline-backward-same-level)
      (outline-backward-same-level arg)
    (outline-indent--advice-backward-same-level 'outline-backward-same-level
                                                arg)))

(defun outline-indent-forward-same-level (&optional arg)
  "Move forward to the ARG'th subheading at same indentation level as this one.
Stop at the first and last indented blocks of a superior indentation."
  (interactive "p")
  (unless arg
    (setq arg 1))
  (if (advice-member-p 'outline-indent--advice-forward-same-level
                       'outline-forward-same-level)
      (outline-forward-same-level arg)
    (outline-indent--advice-forward-same-level 'outline-forward-same-level
                                               arg)))

(defun outline-indent-select ()
  "Select the indented block at point."
  (interactive)
  (outline-indent--deactivate-region)
  (let ((begin (save-excursion
                 (outline-back-to-heading)
                 (point)))
        (end (outline-indent--next-lower-or-equal-indentation)))
    (goto-char (+ end 1))
    (push-mark)
    (goto-char begin)
    (activate-mark)))

(defun outline-indent-close-folds ()
  "Close all folds."
  (interactive)
  (with-no-warnings (outline-hide-sublevels 1)))

(defun outline-indent-open-folds ()
  "Open all folds."
  (interactive)
  (outline-show-all))

;; TODO: Use the original one
(defun outline-indent--legacy-outline-show-entry ()
  "Show the body directly following this heading. (Emacs version.)
Show the heading too, if it is currently invisible."
  (interactive)
  (save-excursion
    (outline-back-to-heading t)
    (outline-flag-region (1- (point))
                         (progn
                           (outline-next-preface)
                           (if (= 1 (- (point-max) (point)))
                               (point-max)
                             (point)))
                         nil)))

(defun outline-indent-open-fold ()
  "Open fold at point."
  (interactive)
  (condition-case nil
      (let ((header-visible (save-excursion
                              (outline-back-to-heading t)
                              (not (outline-invisible-p)))))
        (save-excursion
          (while (outline-indent-folded-p)
            ;; Repeatedly reveal children and body until the entry is no
            ;; longer folded
            (save-excursion
              (outline-back-to-heading)
              (outline-show-children)
              (outline-indent--legacy-outline-show-entry))))

        ;; If the header was previously hidden, hide the subtree to collapse
        ;; it. Otherwise, leave the fold open. This allows the user to decide
        ;; whether to expand the content under the cursor.
        (unless header-visible
          (outline-indent--legacy-outline-hide-subtree)))
    ;; Ignore `outline-before-first-heading'
    (outline-before-first-heading
     nil)))

;; TODO: Use the original one
(defun outline-indent--legacy-outline-hide-subtree (&optional event)
  "Hide everything after this heading at deeper levels.
If non-nil, EVENT should be a mouse event."
  (interactive (list last-nonmenu-event))
  (save-excursion
    (when (mouse-event-p event)
      (mouse-set-point event))
    (outline-flag-subtree t)))

(defun outline-indent-close-fold ()
  "Close fold at point."
  (interactive)
  (condition-case nil
      (save-excursion
        (outline-back-to-heading)
        (if (or (outline-indent-folded-p)  ; Folded?
                ;; Fold without any content
                (let ((start (save-excursion (end-of-line) (point)))
                      (end (save-excursion (outline-end-of-subtree) (point))))
                  (= start end)))
            (progn
              (when (eq (ignore-errors (outline-up-heading 1 t) :success)
                        :success)
                (when (outline-on-heading-p)
                  (outline-indent--legacy-outline-hide-subtree))))
          (outline-indent--legacy-outline-hide-subtree)))
    ;; Ignore `outline-before-first-heading'
    (outline-before-first-heading
     nil)))

(defun outline-indent-open-fold-rec ()
  "Open fold at point recursively."
  (interactive)
  (condition-case nil
      (outline-show-subtree)
    ;; Ignore `outline-before-first-heading'
    (outline-before-first-heading
     nil)))

(defun outline-indent-toggle-fold ()
  "Open or close a fold under point."
  (interactive)
  (outline-toggle-children))

(defun outline-indent-folded-p ()
  "Return non-nil when the current heading is folded."
  (save-excursion
    (outline-back-to-heading)
    (outline-end-of-heading)
    (outline-invisible-p (point))))

(defun outline-indent-close-level (level)
  "Close the folds at the level: LEVEL."
  (outline-hide-sublevels level))

(defun outline-indent-toggle-level-at-point ()
  "Toggle the visibility of the indentation level under the cursor."
  (interactive)
  (when (outline-on-heading-p)
    (let ((level (let ((current-level (outline-indent-level))
                       (next-level (save-excursion (outline-next-heading)
                                                   (outline-indent-level))))
                   (if (< next-level current-level)
                       (+ 1 current-level)
                     next-level))))
      (if (and (outline-on-heading-p)
               (outline-indent-folded-p))
          (outline-hide-sublevels level)
        (outline-hide-sublevels (- level 1))))))

;;; Mode

;;;###autoload
(define-minor-mode outline-indent-minor-mode
  "Toggle `outline-indent-minor-mode'.
This mode sets up outline to work based on indentation."
  :lighter " OutlInd"
  :keymap outline-indent-minor-mode-map
  :group 'outline-indent
  (if outline-indent-minor-mode
      (progn
        (outline-indent--advise-func outline-indent-advise-outline-functions)
        ;; Enable minor mode
        (when (boundp 'outline-minor-mode-highlight)
          (setq-local outline-minor-mode-highlight nil))
        (when (boundp 'outline-search-function)
          (setq-local outline-search-function nil))
        (setq-local outline-heading-alist nil)
        (setq-local outline-level #'outline-indent-level)
        (setq-local outline-heading-end-regexp "\n")
        (setq-local outline-regexp (rx bol
                                       (zero-or-more (any " \t"))
                                       (not (any " \t\n"))))
        (outline-indent--update-ellipsis)
        (outline-indent--setup-basic-offset)
        (outline-minor-mode 1))
    ;; Disable minor mode
    (outline-minor-mode -1)
    (kill-local-variable 'outline-minor-mode-highlight)
    (kill-local-variable 'outline-search-function)
    (kill-local-variable 'outline-heading-alist)
    (kill-local-variable 'outline-level)
    (kill-local-variable 'outline-heading-end-regexp)
    (kill-local-variable 'outline-regexp)))

;;; Provide

(provide 'outline-indent)

;;; outline-indent.el ends here
