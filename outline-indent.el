;;; outline-indent.el --- Fold text using indentation  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1.0.6
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
;; The `outline-indent.el' Emacs package provides a minor mode that enables code
;; folding and outlining based on indentation levels for various
;; indentation-based text files, such as YAML, Python, and other indented text
;; files.
;;
;; In addition to code folding, `outline-indent.el' allows moving indented
;; subtrees up and down, promoting and demoting sections to adjust indentation
;; levels, customizing the ellipsis, and inserting a new line with the same
;; indentation level as the current line, among other features.
;;
;; The `outline-indent.el' package utilizes the built-in outline-minor-mode,
;; which is maintained by the Emacs developers and is less likely to be
;; abandoned like *origami.el* or *yafolding.el*. Since `outline-indent.el' is
;; based on outline-minor-mode, it's also much much faster than origami.el and
;; yafolding.el.

;;; Code:

(require 'outline)

(defgroup outline-indent nil
  "Non-nil if outline-indent mode mode is enabled."
  :group 'outline-indent
  :prefix "outline-indent-")

(defcustom outline-indent-default-offset 1
  "Default indentation offset.
It is used by to determine the outline level based on the current indentation."
  :type 'integer
  :group 'outline-indent)

(defcustom outline-indent-shift-width nil
  "Default shift width for indentation adjustments in promote and demote.
When set to nil, this variable defaults to the value of
`outline-indent-default-offset'.

This setting is used by:
- `outline-indent-demote' (or `outline-demote') to increase the indentation
  level of the subtree.
- `outline-indent-promote' (or `outline-promote') to decrease the indentation
  level of the subtree."
  :type '(choice (const :tag "Use default" nil)
                 integer)
  :group 'outline-indent)

(defcustom outline-indent-ellipsis nil
  "String used as the ellipsis character in `outline-indent-mode'.
When set to nil, the default behavior is not to modify the ellipsis.

The change affects only `outline-indent-minor-mode' (which will then use its own
display table). To apply the change, you need to execute
`outline-indent-minor-mode' in the buffer."
  :type '(choice string (const nil))
  :group 'outline-indent)

(defcustom outline-indent-advise-outline-functions t
  "If non-nil, advises built-in `outline' functions to improve compatibility.
It is highly recommended to keep `outline-indent-advise-outline-functions'
set to t.

If non-nil, advises built-in `outline-minor-mode' functions to improve
compatibility with `outline-indent-minor-mode'.

Functions that will be advised when `outline-indent-minor-mode' is active
include:
- `outline-insert-heading'
- `outline-move-subtree-up'
- `outline-move-subtree-down'

The built-in `outline-minor-mode' functions will work exactly as before and will
only exhibit different behavior when `outline-indent-minor-mode' is active."
  :type 'boolean
  :group 'outline-indent)

(defvar outline-indent-minor-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `outline-indent-minor-mode'.")

(defun outline-indent-level ()
  "Determine the outline level based on the current indentation."
  (+ 1 (/ (current-indentation) (max outline-indent-default-offset 1))))

(defun outline-indent--update-ellipsis ()
  "Update the buffer's outline ellipsis."
  (when outline-indent-ellipsis
    (let* ((display-table (or buffer-display-table (make-display-table)))
           (face-offset (* (face-id 'shadow) (ash 1 22)))
           (value (vconcat (mapcar (lambda (c) (+ face-offset c))
                                   outline-indent-ellipsis))))
      (set-display-table-slot display-table 'selective-display value)
      (setq buffer-display-table display-table))))

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
  (let ((initial-indentation nil)
        (found-point nil))
    (save-excursion
      (beginning-of-visual-line)
      (setq initial-indentation (current-indentation))
      (while (and (not found-point) (not (eobp)))
        (forward-line 1)
        (if (and (>= initial-indentation (current-indentation))
                 (not (looking-at-p "^[ \t]*$")))
            (setq found-point (point))))

      (when (and (not found-point) (eobp))
        (setq found-point (point))))

    (when found-point
      (goto-char found-point)
      (forward-line -1)
      (end-of-line)
      (newline)
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

(defun outline-indent-demote (&optional which arg)
  "Demote the subtree, increasing its indentation level.

The global variable `outline-indent-shift-width' or
`outline-indent-default-offset' is used to determine the number of spaces to
indent the subtree.

WHICH is ignored (backward compatibility with `outline-demote').
If ARG is positive, indent the outline. If ARG is negative, unindent the
outline. Defaults to 1 if ARG is nil."
  (interactive)
  (unless which
    ;; Ignore: Warning: Unused lexical argument `which'
    (setq which t))
  (unless arg
    (setq arg 1))
  (let ((shift-right (>= arg 0))
        (column (current-column))
        (shift-width
         (cond (outline-indent-shift-width
                (max outline-indent-shift-width 1))

               (t
                (max outline-indent-default-offset 1)))))
    (outline-back-to-heading)
    (let ((start (point))
          (end (save-excursion
                 (outline-end-of-subtree)
                 (point)))
          (folded (save-match-data
                    (outline-end-of-heading)
                    (outline-invisible-p))))
      (indent-rigidly start end (if shift-right
                                    shift-width
                                  (* -1 shift-width)))
      (if folded
          (outline-hide-subtree)))
    (if shift-right
        (move-to-column (+ column shift-width))
      (move-to-column (max (- column shift-width) 0)))))

(defun outline-indent-promote (&optional which)
  "Promote the subtree, decreasing its indentation level.
The global variable `outline-indent-shift-width' or
`outline-indent-default-offset' is used to determine the number of spaces to
unindent the subtree.
WHICH is ignored (backward compatibility with `outline-promote')."
  (interactive)
  (unless which
    ;; Ignore: Warning: Unused lexical argument `which'
    (setq which t))
  (outline-indent-demote nil -1))

(defun outline-indent--advice-promote (orig-fun &rest args)
  "Advice function for `outline-indent-promote'.

If `outline-indent-minor-mode' is active, use `outline-indent-insert-heading'.
Otherwise, call the original function with the given arguments.

ORIG-FUN is the original function being advised, and ARGS are its arguments."
  (if (bound-and-true-p outline-indent-minor-mode)
      (outline-indent-promote)
    (apply orig-fun args)))

(defun outline-indent--advice-demote (orig-fun &rest args)
"Advice function for `outline-indent-demote'.

If `outline-indent-minor-mode' is active, use `outline-indent-insert-heading'.
Otherwise, call the original function with the given arguments.

ORIG-FUN is the original function being advised, and ARGS are its arguments."
(if (bound-and-true-p outline-indent-minor-mode)
    (outline-indent-demote)
  (apply orig-fun args)))

(defun outline-indent-move-subtree-down (&optional arg)
  "Move the current subtree down past ARG headlines of the same level.

This function ensures the last blank line is included, even when
`outline-blank-line' is set to t. It also restores the cursor position,
addressing the issue where the cursor might be reset after the operation."
  (interactive "p")
  (unless arg
    (setq arg 1))
  ;; Update 1: Save outline-blank-line
  (let* ((original-outline-blank-line outline-blank-line)
         ;; Update 2: Save the column
         (column (current-column))
         ;; Update 3: Ensure that all empty lines are included
         (outline-blank-line nil))
    (outline-back-to-heading)
    (let* ((movfunc (if (> arg 0) 'outline-get-next-sibling
                      'outline-get-last-sibling))
           ;; Find the end of the subtree to be moved as well as the point to
           ;; move it to, adding a newline if necessary to ensure these points
           ;; are at the beginning of the line below the subtree.
           (end-point-func (lambda ()
                             (outline-end-of-subtree)
                             (if (eq (char-after) ?\n) (forward-char 1)
                               (if (and (eobp) (not (bolp))) (insert "\n")))
                             (point)))
           (beg (point))
           (folded (save-match-data
                     (outline-end-of-heading)
                     (outline-invisible-p)))
           (end (save-match-data
                  (funcall end-point-func)))
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
          (funcall end-point-func))
      (move-marker ins-point (point))
      (insert (delete-and-extract-region beg end))
      (goto-char ins-point)
      (if folded
          ;; Update 4: Hide the subtree using the original outline-blank-line
          ;; This ensures that the fold is closed properly
          (let ((outline-blank-line original-outline-blank-line))
            (outline-hide-subtree)))
      (move-marker ins-point nil))
    ;; Update 5: Restore the column
    (move-to-column column)))

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

;;;###autoload
(define-minor-mode outline-indent-minor-mode
  "Toggle `outline-indent-minor-mode'.
This mode sets up outline to work based on indentation."
  :lighter " OutlInd"
  :keymap outline-indent-minor-mode-map
  :group 'outline-indent
  (if outline-indent-minor-mode
      (progn
        ;; Enable minor mode
        (when (boundp 'outline-minor-mode-highlight)
          (setq-local outline-minor-mode-highlight nil))
        (setq-local outline-heading-alist nil)
        (setq-local outline-level #'outline-indent-level)
        (setq-local outline-heading-end-regexp "\n")
        (setq-local outline-regexp (rx bol
                                       (zero-or-more (any " \t"))
                                       (not (any " \t\n"))))
        (outline-indent--update-ellipsis)

        (when outline-indent-advise-outline-functions
          ;; Advise the built-in `outline-mode' and `outline-minor-mode'
          ;; functions to improve compatibility with
          ;; `outline-indent-minor-mode'. The built-in `outline-minor-mode'
          ;; functions will work exactly as before and will only exhibit
          ;; different behavior when `outline-indent-minor-mode' is active.
          (advice-add 'outline-promote
                      :around #'outline-indent--advice-promote)
          (advice-add 'outline-demote
                      :around #'outline-indent--advice-demote)
          (advice-add 'outline-insert-heading
                      :around #'outline-indent--advice-insert-heading)
          (advice-add 'outline-move-subtree-up
                      :around #'outline-indent--advice-move-subtree-up)
          (advice-add 'outline-move-subtree-down
                      :around #'outline-indent--advice-move-subtree-down))

        (outline-minor-mode 1))
    ;; Disable minor mode
    (outline-minor-mode -1)
    (kill-local-variable 'outline-level)
    (kill-local-variable 'outline-heading-end-regexp)
    (kill-local-variable 'outline-regexp)))

(provide 'outline-indent)

;;; outline-indent.el ends here
