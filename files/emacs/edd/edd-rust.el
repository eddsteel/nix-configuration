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


(provide 'edd-rust)
