(defmacro edd-with-secrets (stub body)
  `(let*
      ((file (expand-file-name (concat "secrets/" ,stub ".el.gpg")
                               user-emacs-directory))
       (exists (file-readable-p file)))

       (if exists
         (progn (load-file file)
                ,body)
         (message (format "Couldn't load %s in secrets dir" ,stub)))))

(defun edd-secrets-get (name)
  "Get an individual secret from keyring or other external command"
  (when (eq 'darwin system-type)
      (letrec
          ((command (string-join `("security find-generic-password -a" ,user-login-name "-s" ,name "-w") " ")))
        (substring (shell-command-to-string command) 0 -1))))

(provide 'edd-secrets)
