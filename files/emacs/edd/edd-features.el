;; Features -- tweaking of stuff that's built-in
;;
;;
(use-package term
  :functions edd/term
  :hook
  (term-mode . edd-term-hook)

  :config
  (defvar edd/term-shell "bash" "Command to run when running term")
  (defun edd/term (pfx)
    (interactive "p")
    "Open my currently favourite kind of terminal, smartly.

     With the prefix argument, opens term.
     If the current buffer is an ansi-term, opens a new one.
     If there's no ansi-term, open a new one.
     Otherwise will switch to *ansi-term*"
    (let ((bn (buffer-name))
          (tl "*ansi-term*")
          (newterm (lambda () (ansi-term edd/term-shell))))
      (if (and (<= pfx 1) (get-buffer tl) (not (string-prefix-p tl bn)))
          (switch-to-buffer tl)
        (funcall newterm))))

  ;; From http://echosa.github.io/blog/2012/06/06/improving-ansi-term
  ;;
  (defun edd-term-hook ()
    (goto-address-mode)
    (define-key term-raw-map (kbd "C-c SPC") 'hydra-music/body)
    (define-key term-raw-map (kbd "C-y") 'edd-term-paste))

  ;; From http://echosa.github.io/blog/2012/06/06/improving-ansi-term
  ;; with an addition: strip space/newlines from the end.
  ;;
  (defun edd-term-paste (&optional string)
    (interactive)
    (process-send-string
     (get-buffer-process (current-buffer))
     (replace-regexp-in-string "[ \n]*\\'" ""
                               (if string string (current-kill 0)))))

  ;; Terminal -- kill on exit
  ;;
  (defadvice term-handle-exit
      (after term-kill-buffer-on-exit activate)
    (kill-buffer))

  :bind (("C-c x" . edd/term)
         :map term-mode-map
         ("M-p" . term-send-up)
         ("M-n" . term-send-down)
         :map term-raw-map
         ("M-o" . other-window)
         ("M-p" . term-send-up)
         ("M-n" . term-send-down)))

(use-package server
  :if window-system
  :config
  (defun edd-features/server-start-unless-running ()
    (unless (server-running-p) (server-start)))
  :hook
  (after-init . edd-features/server-start-unless-running))

(use-package flyspell
  :delight " 💅"
  :hook
  ((text-mode org-mode) . flyspell-mode))

(use-package executable
  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package eldoc :delight " 📜")

(use-package savehist
  :config
  (savehist-mode t)
  (setq savehist-file
        (expand-file-name "history" user-emacs-directory)))

(use-package goto-addr
  :hook
  (((compilation-mode text-mode conf-mode term-mode shell-mode eshell-mode) . goto-address-mode)
   (prog-mode . goto-address-prog-mode))
  :bind
  (:map goto-address-highlight-keymap
        ("C-c C-o" . goto-address-at-point))
  :commands (goto-address-prog-mode
             goto-address-mode))

(use-package ispell
  :config
  ;; use english dictionary (there's no canadian or british one)
  (setq ispell-dictionary "english"))

(use-package imenu
  :bind
  ("M-i" . imenu))

(use-package autorevert
  :delight auto-revert-mode
  :config
  (global-auto-revert-mode))

;; colorize compilation buffers
;; From http://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
(use-package ansi-color
  :config
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :init
    (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer)
  )

(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer))

(use-package subword
  :delight
  :hook
  (prog-mode . subword-mode))

(use-package emacs
  :delight
  (auto-fill-function) (visual-line-mode)
  :init
  (autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
  :config
  ;; do sentences like a normal person
  (setq sentence-end-double-space nil)
  (dolist
      (feature '(upcase-region downcase-region set-goal-column narrow-to-region))
    (put feature 'disabled nil))
  (setq make-backup-files nil)
  ;; custom file
  (setq custom-file
        (expand-file-name "custom.el" user-emacs-directory))
  ;; uniquify buffers
  (setq uniquify-buffer-name-style 'post-forward)
  ;; make C-v M-v symmetrical
  (setq scroll-preserve-screen-position 'always)
  ;; keep system clipboard in kill ring when overwriting it
  (setq save-interprogram-paste-before-kill t)
  ;; full width cursor
  (setq x-stretch-cursor t)
  ;; hurt me plenty
  (setq large-file-warning-threshold 100000000)
  (setq initial-scratch-message
        (concat initial-scratch-message
                "\n(load-file user-init-file)"
                "\n(progn (require \\='restart-emacs) (restart-emacs))"))
  :bind
  (("M-SPC" . cycle-spacing)
   ("M-=" . count-words)
   ("C-x C-m" . execute-extended-command)
   ("M-z" . zap-up-to-char)
   ("C-(" . previous-buffer)
   ("C-)" . next-buffer)
   ("M-o" . other-window)
   ("C-c r" . comint-run)))

(use-package smerge-mode
  :delight " ±")

;; Override _ in ctl-x 8 to provide vowels with macrons
(use-package emacs
  :init
  (with-eval-after-load 'iso-transl
    (progn
      (setcdr (assoc "_a" iso-transl-char-map) [?ā])
      (add-to-list 'iso-transl-char-map '("_i" . [?ī]))
      (add-to-list 'iso-transl-char-map '("_u" . [?ū]))
      (add-to-list 'iso-transl-char-map '("_e" . [?ē]))
      (setcdr (assoc "_o" iso-transl-char-map) [?ō])
      (iso-transl-define-keys iso-transl-char-map))))

(use-package emacs
  :hook
  ((scheme-mode elisp-mode) . my-pretty-lambda)
  :init
  (global-prettify-symbols-mode 1)
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete)
  :config
  (defun my-pretty-lambda ()
    "make some word or string show as pretty Unicode symbols"
    (setq prettify-symbols-alist
          '(("lambda" . 955)))))

(use-package re-builder
  :config
  (setq reb-re-syntax 'string))

(use-package wdired
  :config
  (setq wdired-create-parent-directories t))

(use-package dired-collapse
  :config
  (add-hook 'dired-mode-hook (lambda () (dired-collapse-mode 1))))

(use-package peep-dired
  :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

(use-package restart-emacs)

(use-package project
  :bind
  (:map project-prefix-map
    ("T" . edd-proj/test)
    ("C" . edd-proj/browse-ci))
  :init
  (setq
   project-switch-use-entire-map 't)
  :config
  (defun edd-proj/git-ssh-stub ()
    (let ((output (shell-command-to-string "git remote get-url origin")))
      (string-match
       "[a-z]*@[a-z.]*:\\([-_a-z0-9]*/[-_a-z0-9]*\\)\\(.git\\)?"
       output)
      (match-string 1 output)))

  (defun edd-proj/browse-ci ()
    "Browse current git project/branch in circle CI"
    (interactive)
    (let ((stub (edd-proj/git-ssh-stub))
          (branch (magit-get-current-branch)))
      (browse-url
       (format "https://app.circleci.com/pipelines/github/%s?branch=%s" stub branch))))

  (defun edd-proj/test ()
    "Test current project"
    (interactive)
    (let ((default-directory (project-root (project-current))))
      (if (file-exists-p (expand-file-name "build.gradle.kts"))
          (compile "envchain gradle gradle test")
        (message "don't know how to test this")))))

(provide 'edd-features)
