(defun org-plus-contrib ())

(use-package org  
  :mode ("\\.(org\\|org.txt)\\'" . org-mode)
  :hook ((org-mode . #'turn-on-visual-line-mode)
         (org-mode . (lambda () (org-bullets-mode 1)))
         (org-mode . (lambda () (org-display-inline-images t t)))
         (org-mode . (lambda ()
                       "Beautify Org Checkbox Symbol"
                       (push '("[ ]" . "☐") prettify-symbols-alist)
                       (push '("[X]" . "☑" ) prettify-symbols-alist)
                       (push '("[-]" . "❍" ) prettify-symbols-alist)
                       (prettify-symbols-mode))))

  :commands (org-confluence-export-as-confluence edd-ox-confluence)
  
  :config
  (setq org-html-validation-link nil)
  (setq org-html-head-include-default-style nil)

  (eval-after-load "org"
    (lambda () (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))))
  (setq org-confirm-babel-evaluate nil
        org-edit-src-content-indentation 0
        org-src-fontify-natively t
        org-src-preserve-indentation t
        org-src-tab-acts-natively t
        org-hide-emphasis-markers t
        org-startup-folded nil)
  (setq org-image-actual-width nil)
  (setq org-deck-title-slide-template
        "<h1>%t</h1>
<h2>%a</h2>")
  (setq org-export-allow-bind-keywords t)
  (defun edd-org-babel-code-properties ()
    "inserts some default properties for org-babel. Note you still need :exports per block for github support"
    (interactive)
    (insert "#+PROPERTY:header-args :results output :session :cache yes :tangle yes :comments org :exports both"))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((clojure . t)
     (css . t)
     (ditaa . t)
     (dot . t)
     (emacs-lisp . t)
     (haskell . t)
     (js . t)
;;     (ledger . t)
     (plantuml . t)
     (python . t)
     (ruby . t)
     (shell . t)
     (sqlite . t)))

  (eval-after-load "scala" (lambda () (org-babel-do-load-languages 'org-babel-load-languages '(scala . t))))

  (setq org-imenu-depth 3)
  (setq org-src-fontify-natively t)
  (setq org-log-done t)
  (setq org-hide-leading-stars t)
  (setq org-use-speed-commands t)
  (setq org-ellipsis "…")
  (setq org-special-ctrl-a/e t)

  (setq org-todo-keyword-faces
        '(("DEV" . (:underline t :foreground "#53f2dc" :bold))
          ("STG" . (:underline t :foreground "#ffac4a" :bold))
          ("DEPLOY" . "org-todo")
          ("PRD" . (:underline t :bold))
          ("PR" . "#53f2dc")))

                                        ; Use the CSS file in ~/.emacs.d
  (add-hook 'org-mode-hook
            (lambda ()
              (setq org-html-head
                    (concat
                     "<link rel=\"stylesheet\" href=\""
                     (file-relative-name user-emacs-directory)
                     "edd/org-export.css\">"))))

  ;; those good bullets
  (font-lock-add-keywords 'org-mode
                          '(("^ +\\([-*]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (defun edd-org-split-gh-pr (tag)
    (let*
        ((pieces (split-string tag "#"))
         (project (car pieces))
         (pr (cadr pieces)))
      (string-join (list project "/pull/" pr))))

  (add-to-list 'org-link-abbrev-alist
               '("gh" .  "https://github.com/"))
  (add-to-list 'org-link-abbrev-alist
               '("ghp" . "https://github.com/%(edd-org-split-gh-pr)"))

  (setq org-bullets-bullet-list
        '("​" "​" "​" "​" "​" "​" "​" "​"))

  (defun edd/create-ticket-notes (project number)
    (let ((url (concat "[[j:" project "-" number "]]"))
          (file (concat "~/txt/work-notes/" project "/" number ".org")))
      (find-file file)
      (beginning-of-buffer)
      (insert "#+TITLE:" project "-" number)
      (newline-and-indent)
      (insert "* " url)
      (newline-and-indent)))

  (defun edd/parse-jira-ticket-near-point ()
    (save-excursion
      (backward-word-strictly 3)
      (re-search-forward ".*[^A-Z]\\([A-Z]+\\)-\\([0-9]+\\).*")
      (cons (match-string 1) (match-string 2))))

  (defun edd/create-jira-notes ()
    (interactive)
    (let ((ticket (edd/parse-jira-ticket-near-point)))
      (edd/create-ticket-notes (car ticket) (cdr ticket))))
  
  (load-file
   (expand-file-name
    (concat user-emacs-directory
            "edd/edd-org-options.el")))

  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c b" . org-iswitchb)
   ("C-c c" . org-capture)
   ("C-c j" . org-journal-new-entry))

  :custom-face
  (org-document-title ((nil :height 1.0)))
  (org-agenda-structure ((nil :height 1.0)))
  (org-link ((nil :foreground "#06d8ff")))
  (org-verbatim ((nil :inherit font-lock-keyword-face)))
  (org-block-begin-line ((nil :background "#444444")))
  (org-block-end-line ((nil :background "#444444")))
  (org-block ((nil :background "#444444")))
  (org-checkbox-done-text ((t (:foreground "#71696A" :strike-through t)))))


(use-package weather-metno
  :unless noninteractive
  :commands weather-metno-forecast
  :after org-agenda
  :init
  (setq weather-metno-location-name "Vancouver, Canada"
        weather-metno-location-latitude 49
        weather-metno-location-longitude -123)
  (setq weather-metno-get-image-props
        '(:width 16 :height 16 :ascent center)))

(use-package ob-http
  :unless noninteractive
  :init
  (org-babel-do-load-languages 'org-babel-load-languages '((http . t))))

(use-package ob-async :unless noninteractive)
(use-package ob-kotlin :unless noninteractive)

(use-package org-journal
  :unless noninteractive
  :config
  (setq org-journal-dir "~/txt/journal")
  (setq org-journal-date-format "%A, %d/%m")
  (setq org-journal-file-format "%Y%m%d.org"))

(use-package org-beautify-theme
  :unless noninteractive
  :init
  (load-theme 'org-beautify t)
  :config
  (set-face-font 'org-agenda-structure "Open Sans-12")
  (set-face-font 'org-level-1 "Open Sans-18")
  (set-face-font 'org-level-2 "Open Sans-16"))

(use-package ox-gfm
  :after org
  :unless noninteractive
  :commands org-gfm-export-to-markdown)

(use-package graphviz-dot-mode :unless noninteractive)
(use-package org-bullets :unless noninteractive)

(provide 'edd-org)

