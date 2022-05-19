;; projectile
;; http://endlessparentheses.com/improving-projectile-with-extra-commands.html
;; https://emacs.stackexchange.com/questions/40553/projectile-run-project-without-prompt
;;

(use-package project
  :bind
  (:map project-prefix-map
    ("C" . edd-proj/browse-ci))
  :init
  (setq
   project-switch-use-entire-map 't)
  :config
  (defun edd-proj/git-ssh-stub ()
    (let ((output (shell-command-to-string "git remote get-url origin")))
      (string-match
       "[a-z]*@[a-z.]*:\\([-_a-z0-9]*/[-_a-z0-9]*\\)\\(.git\\)?"
       output
       )
      (match-string 1 output)))

  (defun edd-proj/browse-ci ()
    "Browse current git project/branch in circle CI"
    (interactive)
    (let ((stub (edd-proj/git-ssh-stub))
          (branch (magit-get-current-branch)))
      (browse-url
       (format "https://app.circleci.com/pipelines/github/%s?branch=%s" stub branch))))  
  )

(provide 'edd-proj)
