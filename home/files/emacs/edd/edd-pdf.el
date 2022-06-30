;; It's often useful to split a frame with a PDF and a text buffer (to
;; run examples or take notes).  These functions are useful to
;; navigate a PDF, that's zoomed to fit a column width, where moving
;; forward may been scrolling down, scrolling to the top of the next
;; column or to the next page.

;;(use-package doc-view
;;  :mode ("\\.pdf\\'" . doc-view-mode)
;;  :config
;;  ;; How wide a column is, in char widths.
;;  (defvar edd-pdf-col 100)
;;
;;  ;; How big right margin is
;;  (defvar edd-pdf-mrg-right 10)
;;
;;  ;; How big bottom margin is
;;  (defvar edd-pdf-mrg-bot 10)
;;
;;  (defun edd-pdf-notes ()
;;    "Open the notes for this PDF file."
;;    (interactive)
;;    (let ((notes (replace-regexp-in-string
;;                  ".pdf$" ".org" (buffer-name))))
;;      (progn
;;        (delete-other-windows)
;;        (find-file-other-window notes))))
;;
;;  (defun edd-pdf-2col ()
;;    "Scale the PDF suitably for 2 column reading."
;;    (interactive)
;;    (progn
;;      (doc-view-scale-reset)
;;      (doc-view-enlarge 1.4)
;;      (setq edd-pdf-col 70) ;; TODO actually work this out.
;;      (image-bol nil)))
;;
;;  (defun edd-pdf-1col ()
;;    "Scale the PDF suitably for 1 column reading."
;;    (interactive)
;;    (progn
;;      (doc-view-scale-reset)
;;      (setq edd-pdf-col 100)
;;      (image-bol nil)))
;;
;;  (defun edd-pdf-next-column ()
;;    "Scroll image so view is at the top, and about one screen over"
;;    (interactive)
;;    (progn
;;      (image-scroll-down 500) ; top
;;      (image-forward-hscroll edd-pdf-col)))
;;
;;  (defun edd-pdf-previous-column ()
;;    "Scroll image so view is at the bottom, and about one screen left"
;;    (interactive)
;;    (progn
;;      (image-scroll-up 500) ; bottom
;;      (image-backward-hscroll edd-pdf-col)))
;;
;;  (defun edd-pdf-next-page ()
;;    "Move to next page and scroll to top-left"
;;    (interactive)
;;    (progn
;;      (doc-view-next-page)
;;      (image-bob)))
;;
;;  (defun edd-pdf-previous-page ()
;;    "Move to next page and scroll to bottom-right"
;;    (interactive)
;;    (progn
;;      (doc-view-previous-page)
;;      (image-eob)))
;;
;;  (defun edd-pdf-beginning ()
;;    "Move to first page and scroll to top-left"
;;    (interactive)
;;    (progn
;;      (doc-view-first-page)
;;      (image-bob)))
;;
;;  (defun edd-pdf-end ()
;;    "Move to last page and scroll to bottom-right"
;;    (interactive)
;;    (progn
;;      (doc-view-last-page)
;;      (image-eob)))
;;
;;  (defun edd-pdf-bottom-p ()
;;    "Indicates if the image is scrolled to bottom"
;;
;;    (let* ((image (image-get-display-property))
;;           (threshold edd-pdf-mrg-bot)
;;           (edges (window-inside-edges))
;;           (vscroll (or (image-mode-window-get 'vscroll) 0))
;;           (win-height (- (nth 3 edges) (nth 1 edges)))
;;           (img-height (ceiling (cdr (image-display-size image)))))
;;      (> 0
;;         (- (- img-height (+ win-height vscroll)) threshold))))
;;
;;  (defun edd-pdf-bottom-right-p ()
;;    "Indicates if the image is scrolled to bottom-right."
;;
;;    (let* ((image (image-get-display-property))
;;           (v-threshold edd-pdf-mrg-bot)
;;           (h-threshold edd-pdf-mrg-right)
;;           (edges (window-inside-edges))
;;           (vscroll (or (image-mode-window-get 'vscroll) 0))
;;           (hscroll (or (image-mode-window-get 'hscroll) 0))
;;           (win-width (- (nth 2 edges) (nth 0 edges)))
;;           (win-height (- (nth 3 edges) (nth 1 edges)))
;;           (img-width (ceiling (car (image-display-size image))))
;;           (img-height (ceiling (cdr (image-display-size image)))))
;;      (> 0
;;         (+
;;          (- (- img-height (+ win-height vscroll)) v-threshold)
;;          (- (- img-width (+ win-width hscroll)) h-threshold)))))
;;
;;
;;
;;  ;; TODO last page.
;;  (defun edd-pdf-forward ()
;;    "Move 'forward'. If we're at the end of a page, that means go
;;to next page. If we're at the bottom of the page but not at the
;;end, that means go to the next column. If we're not at the bottom
;;of the page at all that means scroll down."
;;    (interactive)
;;    (cond
;;     ((edd-pdf-bottom-right-p) (edd-pdf-next-page))
;;     ((edd-pdf-bottom-p) (edd-pdf-next-column))
;;     (t
;;      (doc-view-scroll-up-or-next-page))))
;;
;;  (defun edd-pdf-backward ())
;;
;;  (define-key doc-view-mode-map
;;    (kbd "M-n") 'edd-pdf-notes)
;;
;;  (define-key doc-view-mode-map
;;    (kbd "1") 'edd-pdf-1col)
;;
;;  (define-key doc-view-mode-map
;;    (kbd "2") 'edd-pdf-2col)
;;
;;  (define-key doc-view-mode-map
;;    (kbd "SPC") 'edd-pdf-forward)
;;
;;  ;; TODO b backward
;;
;;  (define-key doc-view-mode-map
;;    (kbd "M-<") 'edd-pdf-beginning)
;;
;;  (define-key doc-view-mode-map
;;    (kbd "M->") 'edd-pdf-end)
;;
;;  (define-key doc-view-mode-map
;;    (kbd "n") 'edd-pdf-next-page)
;;
;;  (define-key doc-view-mode-map
;;    (kbd "p") 'edd-pdf-previous-page)
;;
;;  (define-key doc-view-mode-map
;;    (kbd "o") 'other-window)
;;
;;  (add-hook 'doc-view-mode-hook 'edd-pdf-1col))

(use-package pdf-tools
  :init
  (pdf-tools-install))

(provide 'edd-pdf)
