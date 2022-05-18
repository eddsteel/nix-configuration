(appt-activate 1)

(require 'org)

;; Log state changes
(setq org-log-done t)
(setq org-hide-leading-stars t)
(setq org-use-speed-commands t)
(setq org-directory "~/txt/gtd")
(setq org-ellipsis "…")

;; Action state changes
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "|" "DONE(d!)" "DELEGATED(l@)")
        (sequence "BLOCK(b)" "INPROG(i)" "PR(p)" "DEV(v)" "DEPLOY(y)" "STG(s)" "PRD(x)")))

;; Project tags are non-hierarchical
(setq org-tags-exclude-from-inheritance
      '("project"))

;; Refile
;; - calendar (:inbox:)
;; - any project in projects
;; - back to the inbox
;;
(setq org-refile-targets
      `((nil . (:level . 1)) ; current buffer headlines
        ("projects.org" . (:tag . "project"))
        ("projects.org" . (:tag . "inbox"))
        ("projects.org" . (:tag . "target"))
        ("someday.org" . (:level . 1))))

;; Capture to inbox
;;
(setq org-capture-templates
      `(("t" "todo" entry (file+headline "projects.org" "In")
         "* TODO %?\n%a\n")))

(setq org-agenda-files
      (mapcar (lambda (f) (concat org-directory "/" f))
              '("projects.org" "birthdays.org")))

;; Stuck unless
;; - there's a next action
;; - the next action has a date (STAY)
(setq org-stuck-projects
      '("+project/-maybe" ("NEXT" "STAY") ""))

;; Thanks http://stackoverflow.com/questions/10074016/org-mode-filter-on-tag-in-agenda-view
;;
(defun edd/org-agenda-filter-tag (tag)
  "Include only entries that correspond to TAG"
  (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
        (current-headline (or (and (org-at-heading-p)
                                   (point))
                              (save-excursion (org-back-to-heading)))))
    (progn
      (if (member tag (org-get-tags-at current-headline))
          nil
          next-headline))))

(defun edd/go-to-work ()
  (interactive)
  (progn
    (find-file "~/txt/gtd/projects.org")
    (widen)
    (beginning-of-buffer)
    (search-forward-regexp "^* Projects")
    (org-narrow-to-element)))

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
          )
        ("work" . "work things")
        ("ws" "work sup"
         ((tags "work&TODO=\"NEXT\"")
          (tags "work&TODO=\"INPROG\"")
          (tags "work&TODO=\"PR\"")
          (tags "work&TODO=\"DEV\"")
          (tags "work&TODO=\"STG\"")
          (agenda ""
                  ((org-agenda-span 3)
                   (org-agenda-start-day "-1d")
                   (org-agenda-skip-function '(edd/org-agenda-filter-tag "work")))))

         (
          (org-agenda-compact-blocks t))
          (org-agenda-tag-filter "work"))
        ("wa" "work agenda"
         ((agenda ""
                  ((org-agenda-span 3)
                   (org-agenda-start-day "-1d")
                   (org-agenda-skip-function '(edd/org-agenda-filter-tag "work")))))

         ((org-agenda-compact-blocks t))
         (org-agenda-tag-filter "work"))
        ("wS" "standup"
         ((tags "work&TODO=\"DONE\"")
          (tags "work&TODO=\"NEXT\"")
          (tags "work&TODO=\"INPROG\"")
          (tags "work&TODO=\"DEV\"")
          (tags "work&TODO=\"PR\"")
          (tags "work&TODO=\"STG\"")
          (tags "work&TODO=\"PRD\"")
          (agenda ""
                  ((org-agenda-span 2)
                   (org-agenda-start-day "-1d")
                   (org-agenda-skip-function '(edd/org-agenda-filter-tag "work"))))
          (tags "work&TODO=\"TODO\""))
         (
          (org-agenda-compact-blocks t))
          (org-agenda-tag-filter "work"))
        ("wp" "work project list"
         ((tags "+work+project"))
         ((org-show-following-heading nil)
          (org-show-hierarchy-above nil)
          (org-show-context 'minimal))
         )
        ("wn" "work next actions"
         ((tags-todo "work&TODO=\"NEXT\""))
         ((org-agenda-compact-blocks t)))))

(if (eq 'darwin system-type)
  (add-to-list 'org-agenda-custom-commands
               '("I" "Import from ical" agenda "" ((org-agenda-mode-hook (lambda () (edd-mac/agenda-iCal)))))))

(setq org-agenda-default-appointment-duration 60)
(setq org-icalendar-timezone "America/Vancouver")
(setq org-icalendar-use-deadline '(event-if-todo todo-due))
;; diary stuff
(setq org-agenda-diary-file diary-file)
(add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
(add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)
(setq org-agenda-include-diary t)

;; a no-dep "import all my configured calendars" deal
;;
(defun edd/batch-import-calendars ()
  (interactive)
  (require 'edd-secrets (expand-file-name "~/.emacs.d/edd/edd-secrets.el"))
  (edd-with-secrets "gcal"
                      (dolist (pair edd/calendars)
                      (edd/download-calendar (car pair) (cdr pair)))))

;; Download given URL and convert to diary format inside org dir.
;;
;; thanks jeff https://github.com/jstautz/.emacs.d/blob/9b2e405ddc3733630699179697d1c57c9f59032d/init-custom-functions.el#L22
;;
(defun edd/download-calendar (label url)
  (let ((diaryfile (concat org-directory "/" label ".diary"))
        (tmpfile (url-file-local-copy url)))
      (find-file diaryfile)
      (flush-lines "^[& ]")
      (icalendar-import-file tmpfile diaryfile t)
      (kill-buffer (current-buffer))
      (kill-buffer (current-buffer))
      (delete-file tmpfile)))

;; make projects.org export on save (use local hook)
(defun edd-gtd-export-projects-on-save ()
  (add-hook 'after-save-hook #'org-html-export-to-html nil t))

(defun edd-gtd-attach-export-hook ()
  (when (string= buffer-file-name
           (expand-file-name (concat org-directory "/" "projects.org")))
    (edd-gtd-export-projects-on-save)))

(add-hook 'org-mode-hook #'edd-gtd-attach-export-hook)

(provide 'edd-gtd)
