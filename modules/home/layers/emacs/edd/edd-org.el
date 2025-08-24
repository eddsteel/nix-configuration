(defun org-plus-contrib ())

(use-package org
  :mode ("\\.(org\\|org.txt)\\'" . org-mode)
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c b" . org-iswitchb)
   ("C-c c" . org-capture)
   ("C-c j" . org-journal-new-entry))
  :hook
  ((org-mode . turn-on-visual-line-mode)
   (org-mode . edd-org/set-html-styles)
   (org-mode . (lambda () (org-display-inline-images t t)))
   (org-mode . (lambda ()
                 "Beautify Org Checkbox Symbol"
                 (push '("[ ]" . "☐") prettify-symbols-alist)
                 (push '("[X]" . "☑" ) prettify-symbols-alist)
                 (push '("[-]" . "❍" ) prettify-symbols-alist)
                 (prettify-symbols-mode)))
   (org-mode . edd-org/attach-export-hook)
   (diary-list-entries . diary-include-other-diary-files)
   (diary-mark-entries . diary-mark-included-diary-files))

  :init
  (appt-activate 1)

  :config
  (setq
   org-confirm-babel-evaluate nil
   org-directory "~/txt/gtd"
   org-edit-src-content-indentation 0
   org-ellipsis "…"
   org-hide-emphasis-markers t
   org-hide-leading-stars t
   org-html-head-include-default-style nil
   org-html-validation-link nil
   org-log-done t
   org-src-fontify-natively t
   org-src-preserve-indentation t
   org-src-tab-acts-natively t
   org-startup-folded nil
   org-use-speed-commands t
   org-image-actual-width nil
   org-export-allow-bind-keywords t
   org-imenu-depth 3
   org-src-fontify-natively t
   org-log-done t
   org-hide-leading-stars t
   org-use-speed-commands t
   org-special-ctrl-a/e t
   org-agenda-default-appointment-duration 60
   org-icalendar-timezone "America/Vancouver"
   org-agenda-diary-file diary-file
   org-agenda-include-diary t
   org-icalendar-use-deadline '(event-if-todo todo-due)
   )

  (setq org-deck-title-slide-template
        "<h1>%t</h1>
<h2>%a</h2>")

  (setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "|" "DONE(d!)" "DELEGATED(l@)")
        (sequence "BLOCK(b)" "INPROG(i)" "PR(p)" "DEV(v)" "DEPLOY(y)" "STG(s)" "PRD(x)")))
  (setq org-tags-exclude-from-inheritance '("project"))

  (setq org-todo-keyword-faces
        '(("DEV" . (:underline t :foreground "#53f2dc" :bold))
          ("STG" . (:underline t :foreground "#ffac4a" :bold))
          ("DEPLOY" . "org-todo")
          ("PRD" . (:underline t :bold))
          ("PR" . "#53f2dc")))

  (setq org-refile-targets
      `((nil . (:level . 1)) ; current buffer headlines
        ("projects.org" . (:tag . "project"))
        ("projects.org" . (:tag . "inbox"))
        ("projects.org" . (:tag . "target"))
        ("someday.org" . (:level . 1))))

  (setq org-capture-templates
      `(("t" "todo" entry (file+headline "projects.org" "In")
         "* TODO %?\n%a\n")))

  (setq org-agenda-files
        (mapcar (lambda (f) (concat org-directory "/" f))
                '("projects.org" "birthdays.org")))

  (setq org-agenda-custom-commands
      '(("p" "project list"
         ((tags "project"))
         ((org-show-following-heading nil)
          (org-show-hierarchy-above nil)
          (org-show-context 'minimal))
         )
        ("n" "actions"
          ((tags-todo "-work&TODO=\"NEXT\""))
          ((org-agenda-compact-blocks t))
          )))

  (when (eq 'darwin system-type)
      (progn
        (add-hook 'org-load-hook #'edd-mac/agenda-iCal)
        (add-to-list 'org-agenda-custom-commands
                     '("I" "Import from ical" agenda "" ((org-agenda-mode-hook (lambda () (edd-mac/agenda-iCal))))))))

  (defun edd/go-to-work ()
    (interactive)
    (progn
      (find-file "~/txt/gtd/projects.org")
      (widen)
      (beginning-of-buffer)
      (search-forward-regexp "^* Projects")
      (org-narrow-to-element)))

 (setq org-stuck-projects
      '("+project/-maybe" ("NEXT" "STAY") ""))

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
;; TODO
;;     (ledger . t)
     (plantuml . t)
     (python . t)
     (ruby . t)
     (shell . t)
     (sqlite . t)))

  (eval-after-load "scala" (lambda () (org-babel-do-load-languages 'org-babel-load-languages '(scala . t))))

  (add-to-list 'org-src-lang-modes
               '("dot" . graphviz-dot))
  (add-to-list 'org-link-abbrev-alist
               '("gh" .  "https://github.com/"))
  (add-to-list 'org-link-abbrev-alist
               '("ghp" . "https://github.com/%(edd-org-split-gh-pr)"))

  (defun edd-org/set-html-styles ()
    (setq org-html-head
          (concat
           "<link rel=\"stylesheet\" href=\""
           (file-relative-name user-emacs-directory)
           "edd/org-export.css\">")))

  (font-lock-add-keywords 'org-mode
                          '(("^ +\\([-*]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (defun edd-org-split-gh-pr (tag)
    (let*
        ((pieces (split-string tag "#"))
         (project (car pieces))
         (pr (cadr pieces)))
      (string-join (list project "/pull/" pr))))

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

  (defun edd-org/export-projects-on-save ()
    (add-hook 'after-save-hook #'org-html-export-to-html nil t))

  (defun edd-org/attach-export-hook ()
    (when (string= buffer-file-name
                   (expand-file-name (concat org-directory "/" "projects.org")))))

  :custom-face
  (org-document-title ((nil :height 1.0)))
  (org-agenda-structure ((nil :height 1.0)))
  (org-link ((nil :foreground "#06d8ff")))
  (org-verbatim ((nil :inherit font-lock-keyword-face)))
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
  :custom-face
  (org-checkbox ((t (:foreground "#000000", :background "#93a1a1"))))
  :config
  ;;(set-face-font 'org-agenda-structure "Open Sans-12")
  ;;(set-face-font 'org-level-1 "Open Sans-18")
  ;;(set-face-font 'org-level-2 "Open Sans-16")
)

(use-package ox-gfm
  :after org
  :unless noninteractive
  :commands org-gfm-export-to-markdown)

(use-package graphviz-dot-mode :unless noninteractive)
(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode)
  :unless noninteractive
  :init
    (setq org-bullets-bullet-list
        '("​" "​" "​" "​" "​" "​" "​" "​")))

(provide 'edd-org)
