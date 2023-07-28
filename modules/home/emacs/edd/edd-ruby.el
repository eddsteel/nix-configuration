(use-package ruby-mode
  :mode "\\.rbi"
  :config
  (setq ruby-insert-encoding-magic-comment nil))

(use-package web-mode)
(use-package inf-ruby
  :hook (ruby-mode . inf-ruby-minor-mode)
  :commands inf-ruby
  :config)

(use-package string-inflection
  :bind
  ("C-c i i" . string-inflection-all-cycle)
  ("C-c i c" . string-inflection-camelcase))

(use-package rake
  :config
  (setq rake-completion-system 'default)
  :bind
  (:map ruby-mode-map
        ("C-c s r" . rake)
        ("C-c s a" . rake-rerun))
  (:map rake-compilation-mode-map
        ("C-c s r" . rake)
        ("C-c s a" . rake-rerun)))

(provide 'edd-ruby)
