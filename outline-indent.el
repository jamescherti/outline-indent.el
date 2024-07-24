;;; outline-indent.el --- Outline and fold text using indentation  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1.0.1
;; URL: https://github.com/jamescherti/outline-indent.el
;; Keywords: outlines
;; Package-Requires: ((emacs "24.4"))
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

(defun outline-indent--advice-move-subtree-up-down (orig-fun &rest args)
  "Advice for `outline-move-subtree-up' and `outline-move-subtree-down'.

It only changes the behavior when `outline-indent-minor-mode' is active;
otherwise, it calls the original function with the given arguments.

This function ensures that:
- The cursor position is restored after the operation, addressing potential
  cursor displacement issues.

ORIG-FUN is the original function being advised, and ARGS are its arguments."
  (if (bound-and-true-p outline-indent-minor-mode)
      ;; Adjust behavior specific to `outline-indent-minor-mode`
      (let ((column (current-column)))
        (apply orig-fun args)
        (move-to-column column))
    ;; Apply the original function without modification
    (apply orig-fun args)))

(defun outline-indent-move-subtree-up (&optional arg)
  "Move the current subtree up past ARG headlines of the same level."
  (interactive "p")
  (unless arg
    (setq arg 1))
  (if (advice-member-p 'outline-indent--advice-move-subtree-up-down
                       'outline-move-subtree-up)
      (outline-move-subtree-up arg)
    (outline-indent--advice-move-subtree-up-down #'outline-move-subtree-up
                                                 arg)))

(defun outline-indent-move-subtree-down (&optional arg)
  "Move the current subtree down past ARG headlines of the same level."
  (interactive "p")
  (unless arg
    (setq arg 1))
  (if (advice-member-p 'outline-indent--advice-move-subtree-up-down
                       'outline-move-subtree-down)
      (outline-move-subtree-down arg)
    (outline-indent--advice-move-subtree-up-down #'outline-move-subtree-down
                                                 arg)))

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
          (advice-add 'outline-insert-heading
                      :around #'outline-indent--advice-insert-heading)
          (advice-add 'outline-move-subtree-up
                      :around #'outline-indent--advice-move-subtree-up-down)
          (advice-add 'outline-move-subtree-down
                      :around #'outline-indent--advice-move-subtree-up-down)))
    ;; Disable minor mode
    (outline-minor-mode -1)
    (kill-local-variable 'outline-level)
    (kill-local-variable 'outline-heading-end-regexp)
    (kill-local-variable 'outline-regexp)))

(provide 'outline-indent)

;;; outline-indent.el ends here
