(use-package emacs
  :config
  ;; certain builds do otherwise
  ;;
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        ns-function-modifier 'hyper)

  ;; unbind some annoying defaults
  ;;
  (dolist (troublesome '("<f11>" "s-h" "s-z" "C-z" "C-x C-z"))
    (global-unset-key (kbd troublesome)))

  (global-set-key (kbd "<mouse-4>") 'isearch-forward)
  (global-set-key (kbd "<mouse-5>") 'isearch-backward)

  (global-set-key (kbd "s-_") (lambda () (interactive) (insert "—")))
  (global-set-key (kbd "s--") (lambda () (interactive) (insert "–")))

  (eval-after-load "isearch"
    '(progn
      (define-key isearch-mode-map (kbd "<mouse-4>") 'isearch-repeat-forward)
      (define-key isearch-mode-map (kbd "<mouse-5>") 'isearch-repeat-backward)))

  (when (executable-find "gpg2")
    (setq epg-gpg-program "gpg2"))

  (setq ns-use-srgb-colorspace t)
  (defun edd-wibble-font ()
    "Switch GUI font between different sizes (switching between laptop and monitor)"
    (interactive)
    (if (eq (face-attribute 'default :height) 120)
        (set-face-attribute 'default (selected-frame) :height 150)
      (set-face-attribute 'default (selected-frame) :height 120))))

(use-package emms
  :ensure nil
  :after edd-emms
  :config
  (define-emms-simple-player
   afplay '(file) (regexp-opt '(".mp3" ".m4a" ".aac" ".flac")) "afplay")
  (setq emms-player-list (list emms-player-afplay)))

(use-package org
  :commands edd-mac/agenda-iCal
  :hook
  (midnight . edd-mac/agenda-iCal)
  :config
  (defun edd-omi-checked (dir)
    (ignore-errors
      (omi-checked dir)))

  (defun edd-mac/agenda-iCal ()
    "Selects checked calendars in iCal.app and imports them into
the the Emacs diary (hacked to support latest version, from org-mac-iCal)"
    (interactive)

    ;; kill diary buffers then empty diary files to avoid duplicates
    (setq currentBuffer (buffer-name))
    (setq openBuffers (mapcar (function buffer-name) (buffer-list)))
    (omi-kill-diary-buffer openBuffers)
    (with-temp-buffer
      (insert-file-contents diary-file)
      (delete-region (point-min) (point-max))
      (write-region (point-min) (point-max) diary-file))

    ;; determine available calendars
    (setq caldav-folders (directory-files "~/Library/Calendars" 1 ".*caldav$"))
    (setq caldav-calendars nil)
    (mapc
     (lambda (x)
       (setq caldav-calendars (nconc caldav-calendars (directory-files x 1 ".*calendar$"))))
     caldav-folders)

    (setq local-calendars nil)
    (setq local-calendars (directory-files "~/Library/Calendars" 1 ".*calendar$"))

    (setq all-calendars (append caldav-calendars local-calendars))

    ;; parse each calendar's Info.plist to see if calendar is checked in iCal
    (setq all-calendars (delq 'nil (mapcar
                                    (lambda (x)
                                      (edd-omi-checked x))
                                    all-calendars)))


    ;; for each calendar, concatenate individual events into a single ics file
    (with-temp-buffer
      (shell-command "sw_vers" (current-buffer))
      (omi-concat-leopard-ics all-calendars))

    ;; move all caldav ics files to the same place as local ics files
    (mapc
     (lambda (x)
       (mapc
        (lambda (y)
          (rename-file (concat x "/" y) ;
                       (concat "~/Library/Calendars/" y)))
        (directory-files x nil ".*ics$")))
     caldav-folders)

    ;; check calendar has contents and import
    (setq import-calendars (directory-files "~/Library/Calendars" 1 ".*ics$"))
    (mapc
     (lambda (x)
       (when (/= (nth 7 (file-attributes x 'string)) 0)
         (omi-import-ics x)))
     import-calendars)

    ;; tidy up intermediate files and buffers
    (setq usedCalendarsBuffers (mapcar (function buffer-name) (buffer-list)))
    (omi-kill-ics-buffer usedCalendarsBuffers)
    (setq usedCalendarsFiles (directory-files "~/Library/Calendars" 1 ".*ics$"))
    (omi-delete-ics-file usedCalendarsFiles)
    (org-pop-to-buffer-same-window currentBuffer)))

(use-package browse-url
  :init
  (setq browse-url-firefox-program (expand-file-name "~/Applications/Home Manager/Firefox.app/Contents/MacOS/firefox"))
  (setq browse-url-firefox-arguments '())) ;; Just use open

(setq edd-emms/consul-p nil)

(provide 'edd-mac)
