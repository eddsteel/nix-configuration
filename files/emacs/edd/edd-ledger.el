;; Process is:
;; - Paste from online banking sites
;; - Run (edd/ledger-prep-from-paste)
;; - Run appropriate clean function on each line (use macros)
;; - Run clean entries
;; - Run `tac' on the paragraphs
;; - Initialize counter (using function)
;; - M-x ledger-mode
;; - Convert each line to a decorated item using function
;; - whitespace-cleanup
;; - sort paragraphs on whole buffer
;; - undecorate-all
;; - then paste into ledger file
;; - check totals with ledger
;; - use categorise-regulars/next-unknown to fill out details.
;;
;;
(require 'dash)

(use-package ledger-mode
  :mode ("\\.ledger$" "ledger\\.dat$")
  :bind
  (:map ledger-mode-map
        ("C-c C-n" . edd/ledger-next-unknown))
  :config
  (setq ledger-post-auto-adjust-amounts t)
  (defun edd/ledger-clean-vancity (month)
    "Clean an entry pasted from Vancity website
   MONTH is the numeric month e.g. \"02\" for February."
    (save-excursion
      (next-line)
      (delete-indentation)
      (end-of-line)
      (let ((beg (point)))
        (search-forward-regexp "-?\\$")
        (delete-region (match-beginning 0) beg))
      (search-forward "$")
      (goto-char (match-beginning 0))
      (backward-char)
      (kill-line)
      (beginning-of-line)
      (insert "2022-" month "-")
      (forward-char 2)
      (let ((beg (point)))
        (search-forward "	")
        (delete-region (point) beg))
      (insert " ")
      (move-beginning-of-line nil)
      (let ((beg (point)))
        (end-of-line)
        (capitalize-region (point) beg))))

  (defun edd/ledger-clean-visa (month)
    "Clean an entry pasted from Vancity VISA site"
    (save-excursion
      (next-line)
      (kill-whole-line)
      (previous-line)
      (beginning-of-line)
      (insert "2022-" month "-")
      (forward-char 2)
      (kill-line)
      (delete-indentation 4)
      (delete-indentation 4)
      (move-beginning-of-line nil)
      (let ((beg (point)))
        (end-of-line)
        (capitalize-region (point) beg))
      (if (string= "Cr" (current-word))
          (backward-kill-word 2)
        (progn
          (search-backward "$")
          (insert "-")))))

  (defun edd/ledger-line-to-entry (acct)
    (search-forward-regexp "-?\\$")
    (goto-char (match-beginning 0))
    (newline)
    (indent-for-tab-command)
    (insert acct "  ")
    (end-of-line)
    (newline)
    (indent-for-tab-command)
    (insert "???")
    (let ((end (point)))
      (newline)
      (backward-paragraph)
      (indent-region (point) end))
    (forward-paragraph)
    (next-line))

  (defun edd/ledger-initialize-register ()
    (set-register 'edd/ledger-i 1))


  (defun edd/ledger-decorate-for-sorting (acct-num)
    (increment-register 1 'edd/ledger-i)
    (forward-word 2)
    (forward-char)
    (let ((beg (point)))
      (forward-word 1)
      (kill-ring-save beg (point)))
    (insert " " acct-num)
    (yank)
    (insert (format "%03d" (get-register 'edd/ledger-i)))
    (sentence-end)
    (next-line 4)
    (beginning-of-line))

  (defun edd/ledger-next-unknown ()
    (interactive)
    (when (search-forward "???" nil t)
      (back-to-indentation)
      (kill-line)))

  (defun edd/ledger-undecorate-all ()
    (beginning-of-buffer)
    (while (re-search-forward "\\([[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}\\) [[:digit:]]\\{7\\}")
      (replace-match "\\1")))


  (defun edd/ledger-clean-entries ()
    (-map
     (lambda (pair) (beginning-of-buffer)(while (search-forward (car pair) nil 't)(replace-match (cdr pair))))
     '(
       ("Funds Transfer-Online" . "Transfer")
       ("Preauthorized Payment" . "Payment")
       ("Point Of Sale " . "")
       ("Preauthorized Credit " . "")
       ("Preauthorized Payment " . "")
       ("Atm Withdrawal-Interac ." . "ATM")
       (" Jj " . " JJ ")
       (" - THANK" . "")
       ("Www." . "www.")
       ("Sq *" . "")
       ("'S" . "'s")
       ("Charges Applied to Account Periodic Flat Feecapitalise" . "Fee")
       )))

  (defun edd/ledger-line-to-decorated-entry (acct num)
    (edd/ledger-line-to-entry acct)
    (forward-line -4)
    (edd/ledger-decorate-for-sorting num))

  (defvar edd/ledger-regulars '())

  (defun edd/ledger-categorise-regulars ()
    (interactive)
    (save-excursion
      (mapcar
       (lambda (pair)
         (save-excursion
           (let ((term (car pair))
                 (cat (cdr pair)))
             (message "term/cat: %s/%s" term cat)
             (while (search-forward term nil 't)
               (save-excursion
                 (next-line 2)
                 (beginning-of-line)
                 (kill-line)
                 (indent-for-tab-command)
                 (insert cat))))))
       edd/ledger-regulars)))

  (defun edd/ledger-prep-from-paste ()
    (interactive)
    (save-excursion
      (delete-matching-lines "exchange rate")
      (delete-matching-lines "exchange amount")
      (delete-matching-lines "foreign currency")
      )
    )
)

(provide 'edd-ledger)
