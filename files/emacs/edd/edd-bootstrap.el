;; Bootstrap use-package
;;
(require 'use-package)
(use-package delight)
(use-package bind-key)

(defun edd/maybe-load-config (name)
  (let ((conf (locate-user-emacs-file name)))
    (when (file-readable-p conf) (load-file conf))))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(provide 'edd-bootstrap)
