;; projectile
;; http://endlessparentheses.com/improving-projectile-with-extra-commands.html
;; https://emacs.stackexchange.com/questions/40553/projectile-run-project-without-prompt
;;
(use-package projectile-ripgrep)
(use-package helm-make
  :config
  (setq helm-make-completion-method 'ivy)
  (setq helm-make-comint 't))

(use-package projectile
  :demand t
  :init

  (defun edd-proj/magit-and-fetch ()
    (interactive)
    (progn
      (call-interactively #'magit-project-status)
      (call-interactively #'magit-fetch-from-upstream)))

  ;; TODO
  (defun edd-proj/term ()
    (interactive)
    (projectile-run-term "/bin/bash"))

  (defun edd-proj/make-or-compile ()
    (interactive)
    (let ((res (ignore-errors (helm-make-projectile))))
      (if res res
	(projectile-compile-project t))))

  (defun edd-proj/compile-no-prompt ()
    (interactive)
    (let ((compilation-read-command nil))
      (projectile-compile-project nil)))

  (defun edd-proj/test-no-prompt ()
    (interactive)
    (let ((compilation-read-command nil))
      (projectile-test-project nil)))

  (defun edd-proj/run-no-prompt ()
    (interactive)
    (let ((compilation-read-command nil))
      (projectile-run-project nil)))

  (defun edd-proj/modeline ()
    (format " 🏉%s" (projectile-project-name)))

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

  (setq projectile--mode-line " 🏉")
  (setq projectile-mode-line-function #'edd-proj/modeline)

  :config
  (plist-put (alist-get 'gradlew projectile-project-types) 'run-command "./gradlew run")
  (plist-put (alist-get 'gradlew projectile-project-types) 'compile-command "./gradlew compile")

  (defhydra+ hydra-project nil "Project"
    ("!"   projectile-run-command-in-root "command")
    ("&"   projectile-run-async-shell-command-in-root "async command")
    ("A"   projectile-grep "grep")
    ("C"   edd-proj/browse-ci "Browse CI")
    ("E"   project-find-regexp "find regexp (any file under root)")
    ("d"   projectile-find-dir "find directory")
    ("f"   projectile-find-file "find file")
    ("F"   projectile-find-file-other-window "find file other window")
    ("K"   projectile-kill-buffers "kill buffers")
    ("L"   projectile-find-file-dwim-other-window "find-file DWIM other window")
    ("c"   edd-proj/compile-no-prompt "compile")
    ("O"   projectile-find-other-file-other-window "find other file other window")
    ("r"   edd-proj/run-no-prompt)
    ("T"   projectile-toggle-between-implementation-and-test "flip between test and implementation")
    ("V"   projectile-ibuffer "ibuffer")
    ("X"   edd-proj/term "term")
    ("e"   project-find-file "find file (any file under root)")
    ("g"   edd-proj/magit-and-fetch "git fetch origin")
    ("d"   projectile-dired "dired")
    ("kc"  projectile-invalidate-cache "invalidate cache")
    ("kd"  projectile-remove-known-project "remove a project")
    ("kk"  projectile-cache-current-file "cache current file")
    ("ks"  projectile-cleanup-known-projects "cleanup projects")
    ("m"   edd-proj/make-or-compile "run Make target or compile command")
    ("o"   projectile-find-other-file "find other file")
    ("p"   projectile-switch-project "switch project >>")
    ("t"   edd-proj/test-no-prompt "test project")
    ("x"   projectile-run-shell "shell"))

  (setq projectile-switch-project-action
        (lambda ()
          (progn
            (projectile-dired)
            (hydra-project/body))))

  :bind
  (("C-c r" . edd-proj/run-comint)
   :map projectile-mode-map
   ("C-c p" . hydra-project/body)))

(provide 'edd-proj)
