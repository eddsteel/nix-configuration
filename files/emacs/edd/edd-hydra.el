(use-package hydra
  :config

  ;; WANT:

  ;; run a build shell e.g. sbt
  ;; run a repl

  ;; possible to do generic dev:
  ;; - build shell
  ;; - repl
  ;; - compile - projectile supports it but want to override based on project type
  ;; - test - ditto
  ;; - run
  ;; - search docs
  ;; ???

  ;; emms
  (defhydra hydra-music (:columns 3 :timeout 2)
    "Music"
    ("a"   emms-browse-by-album "browse (album)" :exit t)
    ("SPC" emms-pause "play/pause")
    ("i"   emms-show "show info")
    ("I"   emms-show-all "show tags" :exit t)
    ("n"   emms-next "next track")
    ("p"   emms-previous "previous track")
    ("+"   emms-volume-raise "volume up")
    ("-"   emms-volume-lower "volume down")
    ("P"   emms-playlist-mode-switch-buffer "playlist" :exit t)
    ("d"   emms-add-dired "add (dired)" :exit t))

  ;; project. Most of this is filled out in edd/edd-proj.el, some in counsel
  ;; https://github.com/abo-abo/hydra/wiki/Projectile-&-Fixmee
  ;;
  (defhydra hydra-project (:exit t :idle 0.4 :columns 5) "Project"
    ("<ESC>" nil "quit"))

  ;; from wiki
  (defhydra hydra-goto-line (goto-map ""
                                      :pre (linum-mode 1)
                                      :post (linum-mode -1))
    "goto-line"
    ("g" goto-line "go")
    ("m" set-mark-command "mark" :bind nil)
    ("q" nil "quit"))


  (defhydra hydra-mark-modify
    (:columns 3)  "Move/Modify"
    ("p" mc/mark-previous-lines "mark previous line (mc)")
    ("P" mc/mark-previous-like-this-symbol "mark prev match/symbol (mc)")
    ("n" mc/mark-next-lines "mark next line (mc)")
    ("N" mc/mark-next-like-this-symbol "mark next match/symbol (mc)")

    ("a" mc/edit-beginnings-of-lines "edit beginnings of lines (mc)")
    ("e" mc/edit-ends-of-lines "edit beginnings of lines (mc)")

    ("*" mc/mark-all-symbols-like-this-in-defun "mark all in defun (mc)")
    ("m" er/expand-region "expand region (er)")
    (";" er/mark-comment "mark comment (er)")
    ("{" er/mark-defun "mark defun (er)")
    ("." er/mark-next-accessor "mark accessor (er)")

    ("(" er/mark-inside-pairs "mark pair contents (er)")
    (")" er/mark-outside-pairs "mark pair (er)")
    ("'" er/mark-inside-quotes "mark quote contents (er)")
    ("\"" er/mark-outside-quotes "mark quote (er)")

    ("#" mc/insert-numbers "insert numbers (mc)"))


  ;; Thanks http://kitchingroup.cheme.cmu.edu/blog/2015/09/28/A-cursor-goto-hydra-for-emacs/
  (defhydra hydra-goto (:exit nil :columns 4) "goto"

    ("c" avy-goto-char-2 "2 char")
    ("C" avy-goto-char "char")
    ("L" avy-goto-char-in-line "char in line")
    ("w" avy-goto-word-1 "word")
    ("S" avy-goto-subword-1 "subword")
    ("l" avy-goto-line "line")
    ("I" ace-window "window")

    ("i" swiper "swiper" :exit t)

    ("s" isearch-forward "search >")
    ("r" isearch-backward "search <")

    ("nn" flycheck-next-error "next error")
    ("np" flycheck-previous-error "prev error")
    ("nl" list-flycheck-errors "error list" :exit t)
    ("nc" flycheck-compile "refresh errors")

    ("Nn" git-gutter:next-hunk "next git hunk")
    ("Np" git-gutter:previous-hunk "previous git hunk")
    ("Nl" git-gutter:statistic "count git changes")
    ("Ns" git-gutter:popup-hunk "show current git change")

    ("b" ivy-switch-buffer "buffer")
    ("R" counsel-recentf "recentf" :exit t)
    ("P" hydra-project/body "project >>" :exit t))

  (eval-after-load "org"
    '(progn
       (define-key org-mode-map (kbd "C-c SPC") 'hydra-music/body)))

  :bind
  (("C-c SPC" . hydra-music/body)
   ("C-c m" . hydra-mark-modify/body)
   ("C-c p" . hydra-project/body)
   ("C-c s" . sbt-hydra)
   ("C-c o" . hydra-goto/body)))

(provide 'edd-hydra)
