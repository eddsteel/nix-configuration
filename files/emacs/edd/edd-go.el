(use-package go-eldoc
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-errcheck
  :bind
  ("C-c C-b c" . go-errcheck))

(use-package flymake-go
  :bind
  ("M-n" . flycheck-next-error)
  ("M-p" . flycheck-previous-error))

(use-package go-mode
  :init
  (if-let ((gopath (getenv "GOPATH")))
      (prog2
          (setenv "PATH" (concat (getenv "PATH") ":" gopath "/bin"))
          (setq exec-path (append exec-path (list (concat gopath "/bin"))))))
  :config
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-to-list 'load-path "~/src/go/src/github.com/dougm/goflymake")
  (add-hook 'go-mode-hook (lambda ()
                            (setq-local tab-width 4)))
  :bind
  (:map go-mode-map
        ("C-c C-r o" . go-remove-unused-imports)
        ("M-." . godef-jump)
        ("C-M-." . godef-jump-other-window)))

(use-package go-projectile)

(provide 'edd-go)
