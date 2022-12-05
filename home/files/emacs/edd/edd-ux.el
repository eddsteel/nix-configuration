;; theme
;;
(use-package nano-theme
  :custom
  (nano-light-background "#E5DFC5")
  (nano-light-critical "#B65D42")
  (nano-light-strong "#42393C")
  (nano-light-subtle "#CEC8B1")
  (nano-light-popout "#B58A7C")
  :custom-face
  (highlight ((t (:background "#F2EBD0"))))
  (nano-faded ((t (:foreground "#666050"))))
  (nano-faded-i ((t (:background "#545044"))))
  (nano-salient ((t (:foreground "#7CA7B5"))))
  (nano-salient-i ((t (:background "#7CA7B5"))))
  (nano-popout ((t (:foreground "#B58A7C"))))
  (nano-popout-i ((t (:background "#B58A7C"))))
  (nano-mono ((t (:weight light :height 130 :family "Fira Code"))))
  (ahs-plugin-default-face ((t (:background "#B58A7C"))))
  (ahs-plugin-default-face-unfocused ((t (:background "#B58A7C"))))
  :config
  (nano-mode)
  (nano-light)
  (set-frame-font "-*-Fira Code-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1" 't))

(use-package nano-modeline
  :init
  (nano-modeline-mode))

;; TODO: see which of these nano mode obviates
(use-package emacs
  :hook
  (after-make-frame-functions . edd-prep-frame)
  (after-init . edd-frame-hook)
  :config
  (setq visible-bell nil)
  (setq ring-bell-function 'ignore)
  (setq use-short-answers 't)
  (column-number-mode t)
  (show-paren-mode t)

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

(provide 'edd-ux)
