(require 'magit)
(require 'dash)
(require 'hydra)

(defun edd-git/origin-main-or-master ()
  (letrec
      ((origin-branch (or
                       (magit-get "branch" "master" "merge")
                       (magit-get "branch" "main" "merge")))
       (branch (if (string-prefix-p "refs/heads/" origin-branch)
                   (substring origin-branch 11)
                 "master")))
    branch))

(defun edd-git-web-link-capture (&rest args)
  ""
  (let
      ((cmd (apply #'concat (-interpose " " (cons "git web-link" args)))) )
    (message cmd)
    (substring
     (shell-command-to-string cmd) 0 -1)))

(defun edd-git-web-link-remote ()
  "Work out the current remote (uses magit, falls back to 'origin')"
  (or
   (magit-get-remote)
   "origin"))

(defun edd-git-web-link-current-file (&rest args)
  "Derive link for current file in the web provider."
  (apply 'edd-git-web-link-capture
         (append
          args
          (list
           "-r"
           (edd-git-web-link-remote)
           "-p"
           (file-relative-name (buffer-file-name))))))

(defun edd-git-web-link-browse-current-file ()
  "Open current file in the web provider on current branch."
  (interactive)
  (edd-git-web-link-current-file "-o"))

(defun edd-git-web-link-browse-current-file-master ()
  "Open current file in the web provider on main/master."
  (interactive)
  (edd-git-web-link-current-file "-o" "-b" (edd-git/origin-main-or-master)))


(defun edd-git-web-link-current-line (&rest args)
  "Derive link for current line in the web provider."
  (apply 'edd-git-web-link-capture
         (append
          args
          (list
           "-r"
           (edd-git-web-link-remote)
           "-p"
           (file-relative-name (buffer-file-name))
           "-l"
           (int-to-string (line-number-at-pos))))))

(defun edd-git-web-link-browse-current-line ()
  "Open current line in the web provider."
  (interactive)
  (edd-git-web-link-current-line "-o"))

(defun edd-git-web-link-browse-current-line-master ()
  "Open current line in the web provider on master."
  (interactive)
  (edd-git-web-link-current-line "-o" "-b" (edd-git/origin-main-or-master)))

(defun edd-git-web-link-current-region (&rest args)
  "Derive link for current region in the web provider."
  (apply 'edd-git-web-link-capture
         (append
          args
          (list
           "-r"
           (edd-git-web-link-remote)
           "-p"
           (file-relative-name (buffer-file-name))
           "-l"
           (int-to-string (line-number-at-pos (region-beginning)))
           "-m"
           (int-to-string (line-number-at-pos (region-end)))))))

(defun edd-git-web-link-browse-current-region ()
  "Open current region in the web provider."
  (interactive)
  (edd-git-web-link-current-region "-o"))

(defun edd-git-web-link-browse-current-region-master ()
  "Open current region in the web provider."
  (interactive)
  (edd-git-web-link-current-region "-b" (edd-git/origin-main-or-master) "-o"))

(defun edd-git-web-link-commit-at-point ()
  (let ((commit (magit-commit-at-point)))
    (apply 'edd-git-web-link-capture
           (list "-r" (edd-git-web-link-remote) "-c" commit "-o"))))

(defun edd-git-web-link-browse-commit-at-point ()
  "Open the commit at point in the web provider"
  (interactive)
  (edd-git-web-link-commit-at-point))

(defun edd-git-web-link-browse ()
  "Open the current project in the web provider"
  (interactive)
  (edd-git-web-link-capture "-o"))


(defun edd-git-browse-pr (&optional args)
  (interactive)
  (let*
     ((default-directory (project-root (project-current)))
     (pr-list (split-string (shell-command-to-string "hub pr list") "[\n]" t " *"))
     (pr (completing-read "PR #:" pr-list))
     (m (string-match "^#\\([0-9]+\\) *.*" pr))
     (prn (match-string 1 pr)))
  (shell-command (concat "hub pr show " prn))))

(defun edd-git-create-pr (&optional args)
  (interactive)
  (magit-run-git-async "pr" args))

(defun edd-git-create-draft-pr (&optional args)
  (interactive)
  (magit-run-git-async "pr" "-d" args))

(transient-define-prefix edd/magit-prs ()
  ["PRs"
   [("c" "create" edd-git-create-pr)
    ("b" "browse" edd-git-browse-pr)
    ("d" "draft"  edd-git-create-draft-pr)
    ]
   ])

(transient-append-suffix
  'magit-dispatch "G"
  '("@" "PRs" edd-magit-prs))

(bind-key "@" (lambda () (interactive) (edd/magit-prs)) 'magit-mode-map)

(transient-append-suffix
  'magit-dispatch "!"
  '("G" "browse commit at point" edd-git-web-link-browse-commit-at-point))

(bind-key "G" #'edd-git-web-link-browse-commit-at-point 'magit-mode-map)

(transient-append-suffix
  'magit-file-dispatch '(-1 -1)
  [("P" "Previous hunk" git-gutter:previous-hunk)
   ("N" "Next hunk" git-gutter:next-hunk)
   ("S" "Show hunk diff" git-gutter:popup-hunk)])

(transient-append-suffix
  'magit-file-dispatch '(-1 -1)
  [("f" "browse current file" edd-git-web-link-browse-current-file)
   ("l" "browse current line" edd-git-web-link-browse-current-line)
   ("r" "browse current region" edd-git-web-link-browse-current-region)])

(transient-append-suffix
  'magit-file-dispatch '(-1 -1)
  [("F" "browse current file on master" edd-git-web-link-browse-current-file-master)
   ("L" "browse current line on master" edd-git-web-link-browse-current-line-master)
   ("R" "browse current region on master" edd-git-web-link-browse-current-region-master)])

(provide 'edd-git-web-link)
