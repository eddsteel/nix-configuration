(require 'magit)
(require 'projectile)
(require 'dash)
(require 'hydra)

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
  "Open current file in the web provider on master."
  (interactive)
  (edd-git-web-link-current-file "-o" "-d"))

(defun edd-git-web-link-browse-current-file-master ()
  "Open current file in the web provider on master."
  (interactive)
  (edd-git-web-link-current-file "-o" "-b" "master" "-d"))


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
  (edd-git-web-link-current-line "-o" "-d"))

(defun edd-git-web-link-browse-current-line-master ()
  "Open current line in the web provider on master."
  (interactive)
  (edd-git-web-link-current-line "-o" "-b" "master" "-d"))


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
  (edd-git-web-link-current-region "-o" "-d"))

(defun edd-git-web-link-browse-current-region-master ()
  "Open current region in the web provider."
  (interactive)
  (edd-git-web-link-current-region "-b" "master" "-o" "-d"))

(defun edd-git-web-link-commit-at-point ()
  (let ((commit (magit-commit-at-point)))
    (apply 'edd-git-web-link-capture
           (list "-r" (edd-git-web-link-remote) "-c" commit))))

(defun edd-git-web-link-browse-commit-at-point ()
  "Open the commit at point in the web provider"
  (interactive)
  (edd-git-web-link-commit-at-point "-o" "-d"))

(defun edd-git-web-link-browse ()
  "Open the current project in the web provider"
  (interactive)
  (edd-git-web-link-capture "-o"))

(defun edd-git-review ()
  (interactive)
  (git-gutter:set-start-revision "origin/master"))

(defun edd-git-browse-pr (&optional args)
  (interactive)
  (let*
    ((default-directory (projectile-project-root))
     (pr-list (split-string (shell-command-to-string "hub pr list") "[\n]" t " *"))
     (pr (completing-read "PR #:" pr-list))
     (m (string-match "^#\\([0-9]+\\) *.*" pr))
     (prn (match-string 1 pr)))
  (shell-command (concat "hub pr show " prn))))

(defun edd-git-create-pr (&optional args)
  (interactive)
  (magit-run-git-async "pr" args))

(define-transient-command edd-magit-prs ()
  "Extras"
  [["PRs"
    ("c" "create" edd-git-create-pr)
    ("b" "browse" edd-git-browse-pr)
    ]])

(require 'magit)

(transient-insert-suffix
  'magit-dispatch "%"
  '("@" "PRs" edd-magit-prs))

(transient-insert-suffix
  'magit-dispatch "%"
  '("x" "browse" edd-git-web-link-browse))

;; TODO: move to hydra
(defhydra hydra-edd-git-web-link (:exit nil :columns 3)
  ("b" magit-blame "blame" :exit t)
  ("B" magit-blame-popup "blame...")
  ("c" edd-git-review "code review")
  ("C" edd-git-web-link-browse-commit-at-point "browse commit at point")
  ("d" magit-diff-buffer-file "diff")
  ("D" magit-diff-buffer-file-popup "diff..")
  ("f" edd-git-web-link-browse-current-file "browse current file")
  ("F" edd-git-web-link-browse-current-file-master "browse current file on master")
  ("g" magit-log-buffer-file "log")
  ("G" magit-log-buffer-file-popup "log...")
  ("l" edd-git-web-link-browse-current-line "browse current line")
  ("L" edd-git-web-link-browse-current-line-master "browse current line on master")
  ("n" git-gutter:next-hunk "Next hunk")
  ("N" magit-blob-next "Next blob")
  ("p" git-gutter:previous-hunk "Previous hunk")
  ("P" magit-blob-previous "Previous blob")
  ("r" edd-git-web-link-browse-current-region "browse current line")
  ("R" edd-git-web-link-browse-current-region-master "browse current line on master")
  ("s" git-gutter:popup-hunk "show hunk diff")
  ("S" git-gutter:statistic "show current git change stats"))

(provide 'edd-git-web-link)
