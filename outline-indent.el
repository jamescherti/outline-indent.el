;;; outline-indent.el --- Outline and fold text using indentation  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1.0.1
;; URL: https://github.com/jamescherti/outline-indent.el
;; Keywords: outlines
;; Package-Requires: ((emacs "25.1"))
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
;; The `outline-indent-minor-mode' mode leverages `outline-minor-mode' to
;; provide an efficient method for outlining and folding text based on
;; indentation levels.

;;; Code:

(require 'outline)

(defgroup outline-indent nil
  "Non-nil if outline-indent mode mode is enabled."
  :group 'outlines
  :prefix "outline-indent-")

(defcustom outline-indent-default-offset 1
  "Default indentation offset."
  :type 'integer
  :group 'outline-indent)

(defcustom outline-indent-ellipsis nil
  "String used as the ellipsis character in `outline-indent-mode'."
  :type '(choice string (const nil))
  :group 'outline-indent)

(defcustom outline-indent-advise-outline-functions t
  "If non-nil, advises built-in `outline' functions to improve compatibility.
It is highly recommended to keep this set to t.

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
  (/ (current-indentation) (max outline-indent-default-offset 1)))

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
  (let ((initial-point (point))
        (current-indent nil)
        (found nil)
        (eobp nil)
        (new-point nil))
    (save-excursion
      (beginning-of-visual-line)
      (setq current-indent (current-indentation))
      (forward-line 1)
      (while (and (not found) (not (eobp)))
        (if (and (>= current-indent (current-indentation))
                 (not (looking-at-p "^[ \t]*$")))
            (progn
              (setq new-point (point))
              (setq found t))
          (forward-line 1)))
      (when (and (not found) (eobp))
        (setq eobp t)))

    (cond (eobp
           (goto-char (point-max))
           (newline)
           (indent-to current-indent))
          (found (progn (goto-char new-point)
                        (forward-line -1)
                        (end-of-line)
                        (newline)
                        (indent-to current-indent)))
          (t (goto-char initial-point)))))

(defun outline-indent-move-subtree-up (&optional arg)
  "Move the current subtree up past ARG headlines of the same level.

This function ensures the last blank line is included, even when
`outline-blank-line' is set to t. It also restores the cursor position,
addressing the issue where the cursor might be reset after the operation."
  (interactive "p")
  (unless arg
    (setq arg 1))
  (outline-indent-move-subtree-down (- arg)))

(defun outline-indent--shift (&optional arg)
  "Indent or deindent the entire subtree.
If ARG is positive, indent the outline. If ARG is negative, deindent the
outline. Defaults to 1 if ARG is nil.

The global variable `outline-indent-default-offset' is used to determine the
number of spaces to indent or deindent the subtree."
  (unless arg
    (setq arg 1))
  (let ((shift-right (>= arg 0))
        (column (current-column))
        (line (line-number-at-pos))
        (shift-width (if indent-tabs-mode
                         1
                       (max outline-indent-default-offset 1))))
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
    (goto-line line)
    (if shift-right
        (move-to-column (+ column shift-width))
      (move-to-column (max (- column shift-width) 0)))))

(defun outline-indent-demote (&optional which)
  "Demote the subtree, increasing its indentation level.
WHICH is ignored (backward compatibility with `outline-demote')."
  (interactive)
  (outline-indent--shift 1))

(defun outline-indent-promote (&optional which)
  "Promote the subtree, decreasing its indentation level.
WHICH is ignored (backward compatibility with `outline-promote')."
  (interactive)
  (outline-indent--shift -1))

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
  (let* ((original-outline-blank-line outline-blank-line)
         (column (current-column))
         (outline-blank-line nil))
    (outline-back-to-heading)
    (let* ((movfunc (if (> arg 0) 'outline-get-next-sibling
                      'outline-get-last-sibling))
           ;; Find the end of the subtree to be moved as well as the point to
           ;; move it to, adding a newline if necessary, to ensure these points
           ;; are at bol on the line below the subtree.
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
          (let ((outline-blank-line original-outline-blank-line))
            (outline-hide-subtree)))
      (move-marker ins-point nil))
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
        (setq-local outline-minor-mode-highlight nil)
        (setq-local outline-heading-alist nil)
        (setq-local outline-level #'outline-indent-level)
        (setq-local outline-heading-end-regexp "\n")
        (setq-local outline-regexp (rx bol
                                       (zero-or-more (any " \t"))
                                       (not (any " \t\n"))))
        (outline-indent--update-ellipsis)
        (outline-minor-mode 1)

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
                      :around #'outline-indent--advice-move-subtree-down)))
    ;; Disable minor mode
    (outline-minor-mode -1)
    (kill-local-variable 'outline-level)
    (kill-local-variable 'outline-heading-end-regexp)
    (kill-local-variable 'outline-regexp)))

(provide 'outline-indent)

;;; outline-indent.el ends here
