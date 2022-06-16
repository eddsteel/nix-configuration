(use-package emms
  :hook
  (emms-player-started . edd-emms/tell-consul)
  (emms-browser-mode . nano-modeline-emms-browser-mode)
  :commands (emms-smart-browse emms-pause emms-browse-by-album)
  :init
  (setq default-major-mode 'fundamental-mode) ;; shim for emms to work
  (require 'emms-setup)
  (require 'emms-tag-editor)
  (require 'emms-info)
  (require 'emms-info-libtag)

  :custom
  (emms-source-file-default-directory (expand-file-name "~/media/music"))
  (emms-playing-time-display-format " %s")
  (emms-playing-time-display-short-p 1)
  (emms-info-functions '(emms-info-libtag))
  (emms-volume-change-function 'emms-volume-pulse-change)
  (emms-show-format "%s")
  (emms-browser-playlist-info-title-format 'ambrevar/emms-artist-album-track-and-title-format)
  (emms-track-description-function #'edd-emms/info-track-description)

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

  (defun edd-emms/tell-consul ()
    (when edd-emms/consul-p
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

  (defun edd-emms/start-or-previous ()
    (interactive)
    (when emms-player-playing-p
      (emms-stop))
    (when (< emms-playing-time 10)
        (emms-playlist-current-select-previous))
    (emms-start))

  (defun edd-emms/info-track-description (track)
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

  (defun edd-emms/now-playing ()
    (if emms-player-playing-p
                    (format emms-show-format
                            (emms-track-description
                             (emms-playlist-current-selected-track)))
      ""))

  :bind (("<f8>" . emms-pause)
         ("<f7>" . edd-emms/start-or-previous)
         ("<f9>" . emms-next)
         ("C-M-s-p" . emms-playlist-mode-switch-buffer)
         ("C-M-s-n" . emms-browse-by-album)
         (:map emms-browser-mode-map
               ("C-i" . emms-browser-expand-one-level)
               ("<tab>" . emms-browser-expand-one-level))))

(use-package transient
  :bind
  ("C-c SPC" . edd-emms/control)
  :init
  (transient-define-prefix edd-emms/control ()
    ["Music"
     [("SPC" "play/pause"     emms-pause)
      ("a"   "browse (album)" emms-browse-by-album)
      ("n"   "next track"     emms-next)
      ("p"   "previous track" emms-previous)
      ("i"   "show info"      emms-show :if (lambda () emms-player-playing-p))
      ("I"   "show tags"      emms-show-all :if (lambda () emms-player-playing-p))
      ("+"   "volume up"      emms-volume-raise :if (lambda () emms-player-playing-p))
      ("-"   "volume down"    emms-volume-lower :if (lambda () emms-player-playing-p))
      ("P"   "playlist"       emms-playlist-mode-switch-buffer)
      ("d"   "add (dired)"    emms-add-dired :if-mode dired)]]))

(provide 'edd-emms)
