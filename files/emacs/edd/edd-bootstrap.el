;; Bootstrap
;;

;; Don't rely on command line args so that raisers, restart-emacs work
(defun is-nix (dirs path)
  (if (null dirs)
      nil
    (if (string-prefix-p (car dirs) path) (car dirs)
      (is-nix (cdr dirs) path))))

(when (is-nix (list "/nix/store" "/var/run/current-system" (expand-file-name "~/.nix-profile")) (executable-find "emacs"))
  (require 'cl-loaddefs)
  (require 'nix-generated-autoload))

(require 'use-package)
(use-package delight)
(use-package bind-key)

(defun edd/maybe-load-config (name)
  (let ((conf (locate-user-emacs-file name)))
    (when (file-readable-p conf) (load-file conf))))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(add-to-list 'exec-path (expand-file-name "~/.nix-profile/bin"))
(add-to-list 'load-path (locate-user-emacs-file "edd"))

(defvar edd/emms-consul-p t "Whether to do consul stuff with emms")

(provide 'edd-bootstrap)
