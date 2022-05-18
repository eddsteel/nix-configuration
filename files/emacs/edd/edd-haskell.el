(use-package hi2
  :delight
  (hi2-mode))

(use-package haskell-mode
  :delight
  (haskell-doc-mode " 📜")
  :mode "\\.hs\\'"
  :hook
  (haskell-mode . haskell-doc-mode)
  (haskell-mode . turn-on-hi2)
  (haskell-mode . haskell-indentation-mode)
  (haskell-mode . haskell-decl-scan-mode)

  :init
  (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
    (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
    (add-to-list 'exec-path my-cabal-path))
  :config
  (setq
   haskell-tags-on-save t
   haskell-process-suggest-remove-import-lines t
   haskell-process-auto-import-loaded-modules t
   haskell-process-type 'cabal-repl
   haskell-process-log t)
  (when (not (string-match-p (regexp-quote (expand-file-name "~/.local/bin")) (getenv "PATH")))
    (setenv "PATH"
            (concat
             (getenv "PATH") ":" (expand-file-name "~/.local/bin") )))

  :bind
  (
   :map haskell-mode-map
   ("C-c C-l" . haskell-process-load-or-reload)
   ("C-c C-z" . haskell-interactive-switch)
   ("C-c C-n C-t" . haskell-process-do-type)
   ("C-c C-n C-i" . haskell-process-do-info)
   ("C-c C-n C-c" . haskell-process-cabal-build)
   ("C-c C-n c" . haskell-process-cabal)
   ("C-c DEL" . haskell-hoogle)))


'(use-package intero
  :commands (intero-mode intero-mode-blacklist)
  :delight " ‽"
  :hook
  (haskell-mode . intero-mode)
  (haskell-mode . intero-mode-blacklist)
  :init
  (setq intero-blacklist (list (expand-file-name "~/.xmonad")))
  :bind
  (:map intero-mode-map
   ("C-c C-j" . intero-repl)))

(use-package flymake-hlint)

(provide 'edd-haskell)
