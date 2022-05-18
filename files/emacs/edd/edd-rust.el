(use-package rust-mode
  :config
  (defun edd-rust-ivy-function ()
    (interactive)
    (funcall 'swiper "\\bfn "))
  (define-key rust-mode-map (kbd "C-c .") 'edd-rust-ivy-function))

(use-package cargo
  :delight (cargo-minor-mode " 🚢")
  :init
  (add-hook 'rust-mode-hook #'cargo-minor-mode))

(use-package toml)
(use-package toml-mode
  :init
  (add-hook 'toml-mode #'cargo-minor-mode))

(use-package flycheck-rust
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  :bind
  (:map rust-mode-map
        ("M-n" . flycheck-next-error)
        ("M-p" . flycheck-previous-error)))

(use-package ac-racer
  :init
  (setq racer-cmd (expand-file-name "~/.cargo/bin/racer"))
  (setq racer-rust-src-path (expand-file-name "~/src/rustc-1.9.0/src"))
  (add-hook 'rust-mode-hook #'ac-racer-setup)
  :commands (ac-racer-setup))

(provide 'edd-rust)
