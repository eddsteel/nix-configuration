(use-package tuareg)

(use-package merlin
  :init
  ;; Start merlin on ocaml files
  (add-hook 'tuareg-mode-hook 'merlin-mode t)
  (add-hook 'caml-mode-hook 'merlin-mode t)
  ;; Enable auto-complete
  (setq merlin-use-auto-complete-mode 'easy)
  ;; Use opam switch to lookup ocamlmerlin binary
  (setq merlin-command 'opam))
