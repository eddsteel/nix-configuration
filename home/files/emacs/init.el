(setq user-emacs-directory "~/.config/emacs")
(defun edd/maybe-load-config (name)
  (let ((conf (locate-user-emacs-file name)))
    (when (file-readable-p conf) (load-file conf))))

(edd/maybe-load-config "local-pre.el")

(require 'edd-bootstrap (locate-user-emacs-file "edd/edd-bootstrap.el"))

(use-package edd-ux
  :if window-system :unless noninteractive)

(use-package edd-features)

(use-package edd-mac
  :if (eq 'darwin system-type) :unless noninteractive)

(use-package edd-org
  :bind (("C-c w" . edd/go-to-work)))

(use-package whitespace
  :unless noninteractive
  :delight whitespace-mode
  :custom-face
  (whitespace-empty ((t (:background "white"))))
  (whitespace-indentation ((t (:background "white"))))
  (whitespace-trailing ((t (:background "white"))))
  (whitespace-line ((t (:background "#666050" :foreground "#F2EBD0"))))
  (whitespace-big-indent ((t (:background "#B58A7C"))))
  :hook
  (((prog-mode text-mode conf-mode) . whitespace-mode))
  :config
  (setq-default
   whitespace-style '(face trailing tabs empty indentation)
   indent-tabs-mode nil))

(use-package whitespace-cleanup-mode
  :config
  (global-whitespace-cleanup-mode 1))

(use-package corfu
  :custom
  (corfu-cycle t "Enable cycling for `corfu-next'/`corfu-previous'")
  (corfu-auto t "Enable auto completion")
  (corfu-quit-at-boundary t "Automatically quit at word boundary")
  (corfu-quit-no-match t "Automatically quit if there is no match")
  :init
  (global-corfu-mode))

(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t))

(use-package vertico-directory
  :demand t
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)
              ("C-l" . vertico-directory-up))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :init
  (setq completion-styles '(substring orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :bind (("C-M-." . consult-line)
         :map project-prefix-map
         ("a" . consult-ripgrep)
         ("b" . consult-buffer)
         ("I" . edd-proj/idea))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key (kbd "M-."))
  (defun edd-proj/idea ()
    (interactive)
    "Open IDEA in current project"
    (let
        ((root (project-root (project-current))))
      (start-process
       (format "*IDEA %s*" (consult--project-name root))
       nil
       "idea"
       (expand-file-name root)
       )))
  (setq consult-narrow-key "<"))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-c C-o" . embark-export)
   ("M-." . embark-dwim)
   ("<help> B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :init (marginalia-mode))

(use-package edd-util
  :demand t
  :bind
  (("C-w" . edd-util/kill-region-or-backward-kill-word)
   ("C-x k" . edd-util/kill-a-buffer)
   ("M-q" . edd-util/fill-or-unfill-paragraph)))

(use-package pdf-tools
  :init (pdf-tools-install))

(use-package flycheck
  :delight " 🛂"
  :hook
  ((rust-mode go-mode scala-mode ruby-mode) . flycheck-mode)
  ((sbt-file-mode) . (lambda () (flycheck-mode -1))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package auto-highlight-symbol
  :hook (prog-mode . auto-highlight-symbol-mode)
  :delight)

(use-package edd-git :demand t)

(use-package edd-ledger)

(use-package markdown-mode+
  :mode ("\\.apib\\$" . markdown-mode))

(use-package imenu-anywhere
  :hook (emacs-lisp-mode . jcs-use-package)
  :config
  (defun jcs-use-package ()
    (add-to-list
     'imenu-generic-expression
     '("Used Packages"
       "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2))))

;; smoother scrolling
(use-package smooth-scrolling
  :hook ((term-mode comint) . (lambda () (setq-local scroll-margin 0)))
  :custom
  (smooth-scroll-margin 5)
  (scroll-conservatively 101)
  (scroll-preserve-screen-position t)
  (auto-window-vscroll nil)
  (scroll-margin 5))

(use-package quickrun
  :bind
  (("C-c q q" . quickrun)
   ("C-c q r" . quickrun-region)))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

;; wrap-region
(use-package wrap-region
  :delight wrap-region-mode
  :hook ((org-mode latex-mode prog-mode) . wrap-region-mode)
  :config
  (wrap-region-add-wrappers
   '(("*" "*" nil org-mode)
     ("~" "~" nil org-mode)
     ("/" "/" nil org-mode)
     ("=" "=" "+" org-mode)
     ("_" "_" nil org-mode)
     ("/*" "*/" "/" (scala-mode java-mode))
     ("$" "$" nil (org-mode latex-mode)))))

(use-package edd-scala)
(use-package edd-haskell)
(use-package edd-ruby)
(use-package edd-rust)
(use-package edd-kotlin)

(use-package lua-mode)
(use-package cc-mode
  :hook (java-mode-hook . (lambda () (c-set-offset 'statement-cont '++))))
(use-package csv)
(use-package groovy-mode)
(use-package python-mode)
(use-package less-css-mode)
(use-package yaml-mode)
(use-package idris-mode
  :delight (idris-simple-indent-mode)
  :config
  (defun edd/idris-next-hole ()
      (interactive)
      (search-forward " ?" nil t))
  :bind
  (:map idris-mode-map
        ("C-c C-j" . idris-pop-to-repl)
        ("C-c C-f" . edd/idris-next-hole)))

(use-package hcl-mode
  :mode ("\\.tf$" . hcl-mode)
  :config
  (add-to-list 'compilation-error-regexp-alist 'hcl)
  (add-to-list 'compilation-error-regexp-alist-alist
             '(hcl "on \\([^ ]*\\) line \\([0-9]+\\)" 1 2)))

(use-package volatile-highlights
  :delight
  :config
  (volatile-highlights-mode t))

(use-package smartparens
  :delight " 🎷"
  :hook
  ((prog-mode markdown-mode org-mode) . turn-on-smartparens-strict-mode)
  :config
  (require 'smartparens-config)
  (sp-pair "(" ")" :wrap "C-c (")
  (sp-pair "[" "]" :wrap "C-c [")
  (sp-pair "{" "}" :wrap "C-c {")
  (sp-pair "'" "'" :wrap "C-c '")
  (sp-pair "\"" "\"" :wrap "C-c \"")

  (defun edd/rww-paren (&optional arg)
    "rewrap with ()"
    (interactive "p")
    (sp-rewrap-sexp '("(" . ")")))

  (defun edd/rww-bracket (&optional arg)
    "rewrap with []"
    (interactive "p")
    (sp-rewrap-sexp '("[" . "]")))

  (defun edd/rww-brace (&optional arg)
    "rewrap with {}"
    (interactive "p")
    (sp-rewrap-sexp '("{" . "}")))

  :bind
  (:map smartparens-mode-map
        ("C-M-a" . sp-beginning-of-sexp)
        ("C-M-e" . sp-end-of-sexp)

        ("C-M-d"   . sp-down-sexp)
        ("C-M-S-u" . sp-up-sexp)
        ("C-M-S-d" . sp-backward-down-sexp)
        ("C-M-u"   . sp-backward-up-sexp)

        ("C-M-f" . sp-forward-sexp)
        ("C-M-b" . sp-backward-sexp)

        ("C-M-n" . sp-next-sexp)
        ("C-M-p" . sp-previous-sexp)

        ("C-S-f" . sp-forward-symbol)
        ("C-S-b" . sp-backward-symbol)

        ("C-<right>" . sp-forward-slurp-sexp)
        ("C-<left>"  . sp-backward-slurp-sexp)
        ("C-M-<right>" . sp-forward-barf-sexp)
        ("C-M-<left>"  . sp-backward-barf-sexp)

        ("C-M-t" . sp-transpose-sexp)
        ("C-M-k" . sp-kill-sexp)
        ("C-k"   . sp-kill-hybrid-sexp)
        ("M-k"   . sp-backward-kill-sexp)
        ("C-M-w" . sp-copy-sexp)

        ("C-<backspace>" . sp-backward-kill-word)

        ("M-[" . sp-backward-unwrap-sexp)
        ("M-]" . sp-unwrap-sexp)

        ("C-c )"  . edd/rww-paren)
        ("C-c ]"  . edd/rww-bracket)
        ("C-c }"  . edd/rww-brace)
        ("C-x C-t" . sp-transpose-hybrid-sexp)
        :map prog-mode-map
        ;; This conflicts in org mode
        ("M-<left>"  . sp-backward-barf-sexp)
        ("M-<right>" . sp-forward-barf-sexp)))

(use-package anzu
  :delight anzu-mode
  :custom-face
  (anzu-mode-line ((t (:foreground "#F2EBD0" :background "#CEC8B1"))))
  (anzu-match-1 ((t (:foreground "#A7B57C" :background "#CEC8B1"))))
  (anzu-match-2 ((t (:foreground "#7CA7B5" :background "#CEC8B1"))))
  (anzu-match-3 ((t (:foreground "#B58A7C" :background "#CEC8B1"))))
  (anzu-replace-to ((t (:foreground "#B58A7C" :background "#CEC8B1"))))
  (anzu-replace-highlight ((t (:foreground "#B58A7C" :background "#CEC8B1"))))
  :init (global-anzu-mode +1)
  :bind
  (("M-%" . anzu-query-replace)
   ("C-M-%" . anzu-query-replace-regexp)))

(use-package edd-emms)

(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  :custom
  (dumb-jump-default-project (expand-file-name "~/src"))
  (dumb-jump-force-searcher 'rg "Use rg to avoid problems with submodules"))

(use-package gradle-mode)

(use-package protobuf-mode
  :hook
  (protobuf-mode . edd-protobuf/set-style)
  :config
  (defun edd-protobuf/set-style ()
    (c-add-style
     "my-style"
     '((c-basic-offset . 2) (indent-tabs-mode . nil)))))

(use-package hcl-mode
  :mode ("\\.tf$" . hcl-mode))

(use-package dired-collapse
  :config
  (add-hook 'dired-mode-hook (lambda () (dired-collapse-mode 1))))

(use-package wgrep
  :after grep
  :bind
  (:map grep-mode-map
        ("C-x C-q" . wgrep-change-to-wgrep-mode)
        ("C-c C-c" . wgrep-finish-edit)))

(use-package direnv
  :config (direnv-mode))

(use-package browse-at-remote)

(use-package evil-numbers
  :bind
  ("C-c +" . 'evil-numbers/inc-at-pt)
  ("C-c -" . 'evil-numbers/dec-at-pt))

(use-package server
  :init
  (setq server-socket-dir (expand-file-name "~/run/emacs")))

(use-package epa
  :config
  (setq epa-pinentry-mode 'loopback))

(use-package browse-url
  :custom
  (browse-url-browser-function 'browse-url-firefox))

(use-package make-mode
  :mode ("Makefile.inc" . makefile-mode))

(use-package nix-mode
  :config
  (setenv "PATH" (concat (getenv "PATH") ":" "/nix/var/nix/profiles/default/bin")))

(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))

  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package restclient)

(use-package fish-mode)

(use-package emacs
  :bind
  ("<Multi_key>" . edd/transient-compose)
  :config
  (defun edd/transient-compose ()
    (interactive)
    (let ((old-im default-transient-input-method))
      (setq default-transient-input-method "compose")
      (activate-transient-input-method)
      (setq default-transient-input-method old-im))))

(use-package restclient)

(use-package fish-mode)
(use-package multiple-cursors
  :bind
  ("C-;" . mc/mark-next-like-this)
  ("C-:" . mc/mark-all-like-this)
  ("C-c ;" . mc/edit-lines))

(edd/maybe-load-config "local.el")
;; acknowledgements
;;
;; http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html
;; http://sachachua.com/blog/2014/12/emacs-configuration-use-package/
;; http://pages.sachachua.com/.emacs.d/Sacha.html
;; https://github.com/mwfogleman/config/blob/master/home/.emacs.d/michael.org
;; https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md
;; https://github.com/bodil/emacs.d/blob/master
;; https://github.com/shosti/.emacs.d/blob/master/personal/p-jabber.el
;; http://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.
;; http://pragmaticemacs.com/emacs/wrap-text-in-custom-characters/
;; http://lists.madduck.net/pipermail/vcs-home/2013-August/000880.html
;; http://anbasile.github.io/2016/12/02/org-babel-is-cool/
;; https://masteringemacs.org/article/whats-new-in-emacs-28-1
