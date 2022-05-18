(use-package kotlin-mode
  :mode ("build.gradle.kts" . kotlin-mode)
  :bind
  ("C-c i" . edd-kt/sort-imports)
  :config
  (setq kotlin-tab-width 4)
  (setq gradle-use-gradlew nil)
  (setq gradle-gradlew-executable (expand-file-name "gradlew" (projectile-project-root)))
  (defhydra+ hydra-project nil "Project"
    ("m" gradle-execute "execute gradle task"))
  (defun edd-kt/sort-imports ()
    (interactive)
    (let
        ((macro [?\M-< ?\C-s ?i ?m ?p ?o ?r ?t return ?\C-a ?\M-h
          ?\C-n ?\M-% ?i ?m ?p ?o ?r ?t ? ?j ?a ?v ?a return ?i
          ?m ?p ?\[ ?o ?r ?t backspace backspace backspace
          backspace ?o ?r ?t ? ?z ?z ?z ?j ?a ?v ?a return ?!
          ?\M-h ?\C-n ?\C-x ?\C-m ?s ?o ?r ?t ? ?l ?i ?n ?e ?s
          return ?\M-h ?\C-n ?\M-% ?z ?z ?z ?j ?a ?v ?a return ?j
          ?a ?v ?a return ?! ?\C-n]))
      (execute-kbd-macro macro)))

  :init
  (add-to-list
   'compilation-error-regexp-alist
   'kotlin-gradle)
  (add-to-list
   'compilation-error-regexp-alist
   'kotlin-lint)
  (add-to-list
   'compilation-error-regexp-alist-alist
   '(kotlin-gradle
     "^e: \\(.*\\): (\\([0-9]+\\), \\([0-9]+\\))" 1 2 3))
  (add-to-list
   'compilation-error-regexp-alist-alist
   '(kotlin-lint
     "^Lint error > \\(.*\\):\\([0-9]+\\):\\([0-9]+\\):" 1 2 3))
  (projectile-update-project-type
   'gradle
   :project-file "build.gradle.kts"))

(use-package flycheck-kotlin
  :commands flycheck-kotlin-setup
  :after flycheck
  :config
  (flycheck-kotlin-setup))

(provide 'edd-kotlin)
