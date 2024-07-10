;;; outline-indent.el --- Outline and fold text using indentation  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1.0.0
;; URL: https://github.com/jamescherti/outline-indent.el
;; Keywords: outlines
;; Package-Requires: ((emacs "24.3"))
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

(defvar outline-indent-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<return>")
                'outline-indent-insert-heading)
    map)
  "Keymap for `outline-indent-minor-mode'.")

(defun outline-indent-level ()
  "Determine the outline level based on the current indentation."
  (/ (current-indentation) outline-indent-default-offset))

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
          (forward-line 1))))

    (if found
        (progn (goto-char new-point)
               (forward-line -1)
               (end-of-line)
               (newline)
               (indent-to current-indent))
      (goto-char initial-point))))

;;;###autoload
(define-minor-mode outline-indent-minor-mode
  "Toggle `outline-indent-minor-mode'.
This mode sets up outline to work based on indentation."
  :lighter " OutlInd"
  :keymap outline-indent-minor-mode-map
  :group 'outline-indent
  (if outline-indent-minor-mode
      (progn
        (setq-local outline-level #'outline-indent-level)
        (setq-local outline-heading-end-regexp "\n")
        (setq-local outline-regexp (rx bol
                                       (zero-or-more (any " \t"))
                                       (not (any " \t\n"))))
        (outline-indent--update-ellipsis)
        (outline-minor-mode 1))
    (outline-minor-mode -1)
    (kill-local-variable 'outline-level)
    (kill-local-variable 'outline-heading-end-regexp)
    (kill-local-variable 'outline-regexp)))

(provide 'outline-indent)

;;; outline-indent.el ends here
