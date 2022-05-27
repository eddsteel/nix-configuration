(use-package hydra
  :init

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

  (eval-after-load "org"
    '(progn
       (define-key org-mode-map (kbd "C-c SPC") 'hydra-music/body)))

  :bind
  (("C-c SPC" . hydra-music/body)
   ("C-c m" . hydra-mark-modify/body)
   ("C-c s" . sbt-hydra)
   ("C-c o" . hydra-goto/body)))

(provide 'edd-hydra)
