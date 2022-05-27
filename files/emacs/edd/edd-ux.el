;; theme
;;
;;(use-package darkokai-theme
;;  :config
;;  (load-theme 'darkokai t))

(use-package nano-theme
  :config
  (nano-dark))

(use-package nano-modeline
  :config
  (nano-modeline-mode))

(use-package emacs
  :hook
  (after-make-frame-functions . edd-prep-frame)
  (after-init . edd-frame-hook)
  :config
  (tooltip-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (setq visible-bell nil)
  (setq ring-bell-function 'ignore)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (column-number-mode t)
  (show-paren-mode t)

  (add-to-list 'default-frame-alist '(height . 63))
  (add-to-list 'default-frame-alist '(width . 120))
  ;; Modeline
  ;;
  (defvar edd-vc-mode-line
    '(" " (:propertize
           ;; Strip the backend name from the VC status information
           (:eval (let ((backend (symbol-name (vc-backend (buffer-file-name)))))
                    (substring vc-mode (+ (length backend) 2))))
           face font-lock-variable-name-face))
    "Mode line format for VC Mode.")

  (put 'edd-vc-mode-line 'risky-local-variable t)

  (make-face 'mode-line-major-mode-face)
  (make-face 'mode-line-minor-mode-face)

  (set-face-attribute 'mode-line-major-mode-face nil
                      :height 1.0
                      :inherit 'mode-line-face)

  (set-face-attribute 'mode-line-minor-mode-face nil
                      :inherit 'mode-line-face)

  (setq-default mode-line-format
                '("%e"
                  mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote
                  " "
                  mode-line-misc-info
                  mode-line-frame-identification mode-line-buffer-identification " "
                  mode-line-position
                  (vc-mode edd-vc-mode)
                  " "
                  (:propertize mode-name face mode-line-major-mode-face)
                  (:eval (propertize (format-mode-line minor-mode-alist) 'face 'mode-line-minor-mode-face))
                  mode-line-end-spaces))

  ;; C-h for delete
  (define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
  ;; C-z for help, in exchange
  (define-key key-translation-map (kbd "C-z") (kbd "<help>"))

  (defun edd-frame-hook () (edd-prep-frame (car (frame-list))))
  (defun edd-prep-frame (frame)
    (with-selected-frame frame
      (when (display-graphic-p)
        (progn
          (if (eq 'darwin system-type)
              (progn
                ;; fade when inactive
                (set-frame-parameter (selected-frame) 'alpha '(100 80))
                (set-frame-font "-*-Fira Code-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1" 't)
                (put 'default-frame-alist 'alpha '(100 80)))
            (progn
              (set-face-attribute 'default nil :font "Fira Code-13")
              (set-face-attribute 'fixed-pitch nil :font "Fira Code-13")
              ))
          (when (member "Noto Emoji" (font-family-list))
            (set-fontset-font t '(#x1F300 . #x1F6FF) "Noto Emoji"))
          (when (member "Noto Sans CJK JP" (font-family-list))
            (dolist (charset '(kana han symbol cjk-misc bopomofo))
              (set-fontset-font (frame-parameter nil 'font) charset
                                (font-spec :family "Noto Sans CJK JP"))))))))

  (setq custom-safe-themes '("4639288d273cbd3dc880992e6032f9c817f17c4a91f00f3872009a099f5b3f84" default)))

(use-package hl-line
  :hook
  (prog-mode . hl-line-mode))

(use-package time
  :config
  ;; Mode line I like.
  (display-time-mode 1)
  (setq display-time-string-forms
        '((propertize (concat day " " (substring monthname 0 3) " " 24-hours ":" minutes " " load)))))

(use-package battery
  :config
  (display-battery-mode t)
  (setq battery-mode-line-format " %b%p%%"))

(use-package nyan-mode
  :config
  (nyan-mode 1))

(use-package basic-theme
  :commands edd-ux/basic-mode
  :config
  (defun edd-ux/basic-mode ()
    "Get super basic"
    (interactive)
    (enable-theme 'basic)
    (setq-default mode-line-format "")
    (setq mode-line-format "")
    (let ((faces-to-toggle '(mode-line mode-line-inactive mode-line-highlight mode-line-emphasis)))
      (mapcar (lambda (face)
                (set-face-attribute face nil :height 100))
              faces-to-toggle))
    (let* ((sans-font (cond ((x-list-fonts "Lucida Grande") '(:font "Lucida Grande"))
                            ((x-list-fonts "Verdana") '(:font "Verdana"))
                            ((x-family-fonts "Sans Serif") '(:family "Sans Serif"))))
           (background-color (face-background 'default nil 'default))
           (padding `(:line-width 5 :color ,background-color)))
      (custom-theme-set-faces 'org-beautify
                              `(org-level-3 ((t (:box ,padding))))
                              `(org-level-2 ((t (:box ,padding))))
                              `(org-level-1 ((t (:box ,padding))))))))

(provide 'edd-ux)
