(require 'magit)
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

(define-transient-command edd-magit-prs ()
  "Extras"
  [["PRs"
    ("c" "create" edd-git-create-pr)
    ("b" "browse" edd-git-browse-pr)
    ]])

(transient-insert-suffix
  'magit-dispatch "!"
  '("}" "browse commit at point" edd-git-web-link-browse-commit-at-point))


(transient-insert-suffix
  'magit-dispatch "!"
  '("@" "PRs" edd-magit-prs))

(transient-insert-suffix
  'magit-file-dispatch "p"
  '("P" "Previous hunk" git-gutter:previous-hunk))
(transient-insert-suffix
  'magit-file-dispatch "n"
  '("N" "Next hunk" git-gutter:next-hunk))

(transient-insert-suffix
  'magit-file-dispatch "s"
  '("S" "show hunk diff" git-gutter:popup-hunk))

(transient-insert-suffix
  'magit-file-dispatch '(-1 0)
  ["Browse" [
     ("f" "browse current file" edd-git-web-link-browse-current-file)
     ("F" "browse current file on master" edd-git-web-link-browse-current-file-master)
     ("l" "browse current line" edd-git-web-link-browse-current-line)
     ("L" "browse current line on master" edd-git-web-link-browse-current-line-master)
     ("r" "browse current region" edd-git-web-link-browse-current-region)
     ("R" "browse current region on master" edd-git-web-link-browse-current-region-master)
     ]])

;;(transient-get-suffix 'magit-file-dispatch '(-1 0));;


(provide 'edd-git-web-link)
