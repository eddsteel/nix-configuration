(use-package emms
  :load-path "../src/emms/lisp"
  :hook
  (emms-player-started . edd/emms-tell-consul)
  :commands (emms-smart-browse emms-pause emms-browse-by-album)
  :init
  (setq default-major-mode 'fundamental-mode) ;; shim for emms to work
  (require 'emms-setup)
  (setq emms-source-file-default-directory (expand-file-name "~/media/music"))
  (setq emms-playing-time-display-format " %s")
  (setq emms-playing-time-display-short-p 1)

  (require 'emms-tag-editor)
  (require 'emms-info)

  ;; Use only libtag for tagging.
  (require 'emms-info-libtag)
  (setq emms-info-functions '(emms-info-libtag))
  (setq emms-info-libtag-program-name (expand-file-name "~/bin/emms-print-metadata"))
  (setq emms-volume-change-function 'emms-volume-pulse-change)
  (setq emms-show-format "%s")

  :config
  (emms-all)
  (emms-default-players)
  (require 'json)
  ;;; Display album in playlist
  (defun ambrevar/emms-artist-album-track-and-title-format (bdata fmt)
    (concat
     "%i"
     (let ((artist (emms-browser-format-elem fmt "a")))
       (if (not artist)
           "%n"                    ; If unknown, display the filename.
         (concat
          "%a - "
          (let ((album (emms-browser-format-elem fmt "A")))
            (if album "%A - " ""))
          (let ((disc (emms-browser-format-elem fmt "D")))
            (if (and disc (not (string= disc ""))) "%D/" ""))
          (let ((track (emms-browser-format-elem fmt "T")))
            (if (and track (not (string= track "0")))
                "%T. "
              ""))
          "%t [%d]")))))
  (setq emms-browser-playlist-info-title-format 'ambrevar/emms-artist-album-track-and-title-format)

;; Display disc number in browser
(defun ambrevar/emms-browser-track-artist-and-title-format (bdata fmt)
  (concat
   "%i"
   (let ((disc (emms-browser-format-elem fmt "D")))
     (if (and disc (not (string= disc "")))
         "%D/"))
   (let ((track (emms-browser-format-elem fmt "T")))
     (if (and track (not (string= track "0")))
         "%T. "
       ""))
   "%n"))
(setq emms-browser-info-title-format 'ambrevar/emms-browser-track-artist-and-title-format)
  (defun edd/emms-modeline ()
    (concat " 🎶 "
            (let ((s (emms-track-get (emms-playlist-current-selected-track) 'info-title
                                     (emms-mode-line-playlist-current))))
              (substring s
                         0 (min 20 (length s))))))

  (defun edd/emms-tell-consul ()
    (when edd/emms-consul-p
      (let*
          ((artist (emms-track-get (emms-playlist-current-selected-track) 'info-artist))
           (title (emms-track-get (emms-playlist-current-selected-track) 'info-title))
           (album (emms-track-get (emms-playlist-current-selected-track) 'info-album))
           (time (string-to-number (format-time-string "%s000")))
           (json (json-encode-alist
                  (list (cons :artist artist)
                        (cons :title title)
                        (cons :album album)
                        (cons :time time)))))
        (start-process "np" "*tell-consul*" "b" "np" "set" json))))

  (setq emms-mode-line-mode-line-function 'edd/emms-modeline)
  (defun edd/emms-start-or-previous ()
    (interactive)
    (when emms-player-playing-p
      (emms-stop))
    (when (< emms-playing-time 10)
        (emms-playlist-current-select-previous))
    (emms-start))

  (defun edd/emms-info-track-description (track)
    "Return a description of TRACK."
    (let ((artist (emms-track-get track 'info-artist))
          (title  (emms-track-get track 'info-title)))
      (cond
       ((and artist title)
        (concat artist " — " title))
       (title
        title)
       (t
        (emms-track-simple-description track)))))
  (setq emms-track-description-function #'edd/emms-info-track-description)

  ;; This is like (emms-show) but doesn't display or insert, just returns the string
  (defun edd/emms-now-playing ()
    (if emms-player-playing-p
                    (format emms-show-format
                            (emms-track-description
                             (emms-playlist-current-selected-track)))
      ""))

  (setq emms-mode-line-mode-line-function 'edd/emms-modeline)
  :bind (("<f8>" . emms-pause)
         ("<f7>" . edd/emms-start-or-previous)
         ("<f9>" . emms-next)
         ("C-M-s-p" . emms-playlist-mode-switch-buffer)
         ("C-M-s-n" . emms-browse-by-album)))

(provide 'edd-emms)
