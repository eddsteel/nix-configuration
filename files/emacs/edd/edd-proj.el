(use-package project
  :bind
  (:map project-prefix-map
    ("T" . edd-proj/test)
    ("C" . edd-proj/browse-ci))
  :init
  (setq
   project-switch-use-entire-map 't)
  :config
  (defun edd-proj/git-ssh-stub ()
    (let ((output (shell-command-to-string "git remote get-url origin")))
      (string-match
       "[a-z]*@[a-z.]*:\\([-_a-z0-9]*/[-_a-z0-9]*\\)\\(.git\\)?"
       output)
      (match-string 1 output)))

  (defun edd-proj/browse-ci ()
    "Browse current git project/branch in circle CI"
    (interactive)
    (let ((stub (edd-proj/git-ssh-stub))
          (branch (magit-get-current-branch)))
      (browse-url
       (format "https://app.circleci.com/pipelines/github/%s?branch=%s" stub branch))))

  (defun edd-proj/test ()
    "Test current project"
    (interactive)
    (let ((default-directory (project-root (project-current))))
      (if (file-exists-p (expand-file-name "build.gradle.kts"))
          (compile "envchain gradle gradle test")
        (message "don't know how to test this")))))

(provide 'edd-proj)
