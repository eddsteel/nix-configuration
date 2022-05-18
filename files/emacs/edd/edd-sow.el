;;; sow.el --- Variable commands for scrolling the other window.

;; Copyright (C) 2016  Andreas Politz

;; Author: Andreas Politz <politza@fh-trier.de>
;; Keywords: extensions, frames

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


(defvar-local edd-sow-scroll-up-command nil)

(defvar-local edd-sow-scroll-down-command nil)

(defvar edd-sow-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km [remap scroll-other-window] 'edd-sow-scroll-other-window)
    (define-key km [remap scroll-other-window-down] 'edd-sow-scroll-other-window-down)
    km)
  "Keymap used for `edd-sow-mode'")

(define-minor-mode edd-sow-mode
  "FIXME: Not documented."
  nil nil nil
  :global t)

(defun edd-sow-scroll-other-window (&optional arg)
  (interactive "P")
  (edd-sow--scroll-other-window-1 arg))

(defun edd-sow-scroll-other-window-down (&optional arg)
  (interactive "P")
  (edd-sow--scroll-other-window-1 arg t))

(defun edd-sow--scroll-other-window-1 (n &optional down-p)
  (let* ((win (other-window-for-scrolling))
         (cmd (with-current-buffer (window-buffer win)
                (if down-p
                    (or edd-sow-scroll-down-command #'scroll-up-command)
                  (or edd-sow-scroll-up-command #'scroll-down-command)))))
    (with-current-buffer (window-buffer win)
      (save-excursion
        (goto-char (window-point win))
        (with-selected-window win
          (funcall cmd n))
        (set-window-point win (point))))))

(add-hook 'Info-mode-hook
          (lambda nil
            (setq edd-sow-scroll-up-command
                  (lambda (_) (Info-scroll-up))
                  edd-sow-scroll-down-command
                  (lambda (_) (Info-scroll-down)))))

(add-hook 'doc-view-mode-hook
          (lambda nil
            (setq edd-sow-scroll-up-command
                  'doc-view-scroll-up-or-next-page
                  edd-sow-scroll-down-command
                  'doc-view-scroll-down-or-previous-page)))

(add-hook 'pdf-view-mode-hook
          (lambda nil
            (setq edd-sow-scroll-up-command
                  'pdf-view-scroll-up-or-next-page
                  edd-sow-scroll-down-command
                  'pdf-view-scroll-down-or-previous-page)))

(provide 'edd-sow)
;;; scroll-other-window.el ends here
