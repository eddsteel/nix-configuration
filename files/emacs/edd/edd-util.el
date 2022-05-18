(defun kill-region-or-backward-kill-word (&optional arg region)
  "`kill-region' if the region is active, otherwise `backward-kill-word'"
  (interactive (list (prefix-numeric-value current-prefix-arg) (use-region-p)))
  (if region
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

(defun edd-initial-file-or-scratch ()
  (interactive)
  "Finds the initial buffer choice file, or if that is nil, the scratch buffer"
  (if initial-buffer-choice
      (find-file initial-buffer-choice)
    (switch-to-buffer "*scratch*")))

(defun edd-hex-encode (start end)
  (interactive "r")
  "replace the region with a URL encoded version"
  (let* ((text (buffer-substring-no-properties start end))
         (encoded (url-hexify-string text)))
    (delete-region start end)
    (insert encoded)))

;; From https://github.com/sri/dotfiles/blob/master/emacs/emacs.d/my-fns.el#L236
;;
(defun edd-sudo-ff ()
  "Use TRAMP to `sudo' the current buffer"
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when file-name
      (find-alternate-file (concat "/sudo::" file-name)))))

;; Exercism submit from project root
(with-eval-after-load "projectile"
  (defun edd-ex-submit ()
    (interactive)
    (let ((file (buffer-file-name))
          (default-directory (projectile-project-root)))
      (display-buffer (process-buffer
     (start-process "exercism" "*exercism*" "exercism" "submit" file))))))

;; This is rad, copied from
(defun edd-fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'edd-fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(global-set-key [remap fill-paragraph]
                #'edd-fill-or-unfill)

;; From https://www.emacswiki.org/emacs/ReverseParagraphs
;;
(defun edd-reverse-paragraphs (beg end)
  "Reverse the order of paragraphs in a region.
From a program takes two point or marker arguments, BEG and END."
  (interactive "r")
  (when (> beg end)
    (let (mid) (setq mid end end beg beg mid)))
  (save-excursion
    ;; the last paragraph might be missing a trailing newline
    (goto-char end)
    (setq end (point-marker))
    ;; the real work.
    (goto-char beg)
    (let (paragraphs fix-newline)
      (while (< beg end)
        ;; skip to the beginning of the next paragraph instead of
        ;; remaining on the position separating the two paragraphs
        (when (= 0 (forward-paragraph 1))
          (goto-char (1+ (match-end 0))))
        (when (> (point) end)
          (goto-char end))
        (setq paragraphs (cons (buffer-substring beg (point))
                               paragraphs))
        (delete-region beg (point)))
      ;; if all but the last paragraph end with two newlines, add a
      ;; newline to the last paragraph
      (when (and (null (delete 2 (mapcar (lambda (s)
                                           (when (string-match "\n+$" s -2)
                                             (length (match-string 0 s))))
                                         (cdr paragraphs))))
                 (when (string-match "\n+$" (car paragraphs) -2)
                   (= 1 (length (match-string 0 (car paragraphs))))))
        (setq fix-newline t)
        (setcar paragraphs (concat (car paragraphs) "\n")))
      ;; insert paragraphs
      (dolist (par paragraphs)
        (insert par))
      (when fix-newline
        (delete-char -1)))))

;; From http://zck.me/emacs-move-file
;;
(defun edd-move-file (new-location)
  "Write this file to NEW-LOCATION, and delete the old one."
  (interactive (list (if buffer-file-name
                         (read-file-name "Move file to: ")
                       (read-file-name "Move file to: "
                                       default-directory
                                       (expand-file-name (file-name-nondirectory (buffer-name))
                                                         default-directory)))))
  (when (file-exists-p new-location)
    (delete-file new-location))
  (let ((old-location (buffer-file-name)))
    (write-file new-location t)
    (when (and old-location
               (file-exists-p new-location)
               (not (string-equal old-location new-location)))
      (delete-file old-location))))

;; From https://www.emacswiki.org/emacs/CamelCase
(defun mapcar-head (fn-head fn-rest list)
      "Like MAPCAR, but applies a different function to the first element."
      (if list
          (cons (funcall fn-head (car list)) (mapcar fn-rest (cdr list)))))

(defun camelize (s)
  "Convert under_score string S to CamelCase string."
  (mapconcat 'identity (mapcar
                        #'(lambda (word) (capitalize (downcase word)))
                        (split-string s "_")) ""))

(defun camelize-method (s)
  "Convert under_score string S to camelCase string."
  (mapconcat 'identity (mapcar-head
                        '(lambda (word) (downcase word))
                        '(lambda (word) (capitalize (downcase word)))
                        (split-string s "_")) ""))


(defun edd/snake-to-camel (start end)
  (interactive "r")
  (letrec ((snake (buffer-substring start end))
        (camel (camelize-method snake)))
    (save-excursion
      (delete-region start end)
      (goto-char start)
      (insert camel))))

(defvar edd/book nil "Relevant book (usually set in dir locals)")
(defun edd/open-book ()
  (interactive)
  (when edd/book (find-file-other-window edd/book)))

;; kill current buffer by default
;; http://irreal.org/blog/?p=5585
(defun edd-kill-a-buffer (askp)
  (interactive "P")
  (if askp
      (kill-buffer (funcall completing-read-function
                            "Kill buffer: "
                            (mapcar #'buffer-name (buffer-list))))
    (kill-this-buffer)))

(defun edd/dashify ()
    (interactive)
    (save-excursion
      (beginning-of-buffer)
      (while (search-forward " -- " nil t) (replace-match " — "))))

(defun edd-wibble-font ()
  "Switch GUI font between different sizes (switching between laptop and monitor)"
  (interactive)
  (if (eq (face-attribute 'default :height) 90)
      (set-face-attribute 'default (selected-frame) :height 70)
    (set-face-attribute 'default (selected-frame) :height 90)))

(provide 'edd-util)
