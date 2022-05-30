(use-package emacs
  ;; sbt env file plugin
  :config
  (add-to-list 'auto-mode-alist '("\\.env\\'" . sh-mode)))

(use-package sbt-mode)
(use-package scala-mode
  :mode (("\\.scala\\'" . scala-mode) ("\\.sc\\'" . scala-mode))
  :hook
  (scala-mode . edd-scala/hook)
  :bind
  (
   :map scala-mode-map
   ("C-c C-b a" . edd-sbt-assembly)
   ("C-c C-v C-l" . edd-sbt-test-only-last)
   ("C-c C-v C-t" . edd-sbt-test-only)
   ("C-c C-b C-l" . sbt-run-previous-command))
  :init
  (defun edd-scala/hook ()
    (progn
      (eval-after-load "counsel" (setq-local counsel-grep-swiper-limit 1200))))
  :config
  (setenv "COURSIER_NO_TERM" "true")
  (setq scala-indent:align-parameters nil
	scala-indent:align-forms nil
	scala-indent:use-javadoc-style)

  (defun edd-run-scala ()
    (interactive)
    (if (ensime-connection-or-nil)
	(call-interactively 'ensime-inf-run-scala)
      (if (sbt:find-root)
	  (run-scala)
	(comint-run "amm"))))
  (defun edd-scala-package-containing-point ()
    (save-excursion
      (let ((segs '()))
	(while (search-backward-regexp
		"^package \\(\\(?:[a-z0-9_]+\\.\\)*[a-z0-9)]+\\)"
		(point-min) t)
	  (let ((segment (match-string 1)))
	    (add-to-list 'segs (ensime-kill-txt-props segment))))
	(mapconcat 'identity segs "."))))

  (defun edd-scala-class-or-module-containing-point ()
    (save-excursion
      (when (search-backward-regexp
	     "^\\(?:class\\|object\\) \\(\\(?:[A-Z]+\\)[a-zA-Z0-9_]*\\)"
	     (point-min) t)
	(let  ((match (match-string 1)))
	  (ensime-kill-txt-props match)))))

  (defun edd-scala-fqn-containing-point ()
    (let ((n (edd-scala-class-or-module-containing-point))
	  (p (edd-scala-package-containing-point)))
      (concat p "." n)))

  (defun edd-sbt-assembly ()
    (interactive)
    (sbt-command "assembly"))

  (defun edd-sbt-test-only ()
    (interactive)
    (letrec ((n (edd-scala-fqn-containing-point))
	     (cmd (concat "test-only " n " -- -oDF")))
      (when n
	(setq edd-scala-last-test-only n)
	(message (concat "sbt " cmd)
		 (sbt-command cmd)))))

  (defun edd-sbt-test-only-last ()
    (interactive)
    (when edd-scala-last-test-only
      (sbt-command (concat "test-only " edd-scala-last-test-only))))

  (defun edd-scala-ivy-method ()
    (interactive)
    (funcall 'swiper "\\bdef "))

  (define-key scala-mode-map (kbd "C-c .") 'edd-scala-ivy-method)

  (defvar edd-scala/root-package "com.eddsteel" "Common root for scala packages")

  (defvar edd-scala/sort-imports-rules
    (list
       (cons (concat "^import " (regexp-quote (concat edd-scala/root-package "."))) "1")
	(cons "^import scala\\." "6")
	(cons "^import java\\." "7")
	(cons "^import javax\\." "8")
	(cons "^import " "5"))
    "Rules for sorting imports of the form (REGEXP . PRECEDENCE)")

;; TODO: support blank lines as both a rule and while traversing
  (defun edd-scala-sort-imports ()
    "Sorts imports according to rules, which are cons pairs of regexp to order"
    (interactive)
    (save-excursion
      (beginning-of-buffer)
      (let ((importre "^import\\b")
	    (blankre  "^$"))
	(search-forward-regexp "^package\\b")
	(search-forward-regexp importre)
	(beginning-of-line)
	(let ((start (point)))
	  (while (looking-at-p importre)
	    (forward-line))
	  (end-of-line)
	  (let ((end (point)))
	    (mapcar
	     (lambda (pair)
	       (progn
		 (goto-char start)
		 (while (< (point) end)
		   (let ((rule (car pair))
			 (ord (cdr pair)))
		     (when
			 (search-forward-regexp rule end 't)
		       (replace-match (concat ord "\\&")))
		     (forward-line)))))
	     edd-scala/sort-imports-rules)
	    (sort-lines nil start end)
	    (goto-char start)
	    (while (search-forward-regexp "^\\([0-9]\\)import\\b" nil 't)
	      (replace-match "import")))))))

  (defun edd-scala-ignore-style (start end)
    "Ignore a scalastyle rule. If region is active it will be
   wrapped in a scalastyle:off/scalastyle:on comment pair. If not, a
   scalastyle:ignore comment will be used."
    (interactive "r")
    (let ((rule (completing-read "style-rule "
				 '("file.size.limit" "line.size.limit" "line.contains.tab" "header.matches" "newline.at.eof" "no.newline.at.eof" "regex" "whitespace.end.of.line" "class.name" "covariant.equals" "cyclomatic.complexity" "equals.hash.code" "if.brace" "illegal.imports" "magic.number" "method.length" "method.name" "no.clone" "no.finalize" "no.whitespace.after.left.bracket" "no.whitespace.before.left.bracket" "null" "number.of.methods" "number.of.types" "object.name" "package.object.name" "parameter.number" "public.methods.have.type" "return" "simplify.boolean.expression" "spaces.after.plus" "spaces.before.plus" "structural.type" "uppercase.l" "var.field" "var.local" "while"))))
      (save-excursion
	(if (region-active-p)
	    (progn
	      (goto-char end)
	      (end-of-line)
	      (newline)
	      (insert "// scalastyle:on " rule)
	      (indent-according-to-mode)
	      (goto-char start)
	      (beginning-of-line)
	      (open-line 1)
	      (insert "// scalastyle:off " rule)
	      (indent-according-to-mode))
	  (progn
	    (end-of-line)
	    (insert " // scalastyle:ignore " rule)))))))


;; Extra mode for .sbt files to stop them being covered in errors.
;;
(define-derived-mode sbt-file-mode scala-mode "SBT file mode"
  "A mode for editing .sbt files")
(add-to-list 'auto-mode-alist '("\\.sbt\\'" . sbt-file-mode))

(defun edd-align-sbt-deps ()
  (interactive)
  (align-regexp (region-beginning) (region-end) "\\(\\s-+\\)\\(%%?\\|\"\\)" 1 1 't))

(defun edd-java-hook ()
  (setq compile-command "ant \-emacs compile \-find")
  (local-set-key (kbd "C-x C-k") 'recompile))

(add-hook 'java-mode-hook 'edd-java-hook)

(require 's)

(defun edd-scala/pkgize-file (file)
  (s-replace "-" "" (s-replace "/" "." file)))

(defun edd-scala/fileize-pkg (pkg)
  (s-replace "." "/" pkg))

(defun edd-scala/fix-sbt-root ()
  (interactive)
  (setq sbt:buffer-project-root (sbt:find-root))
  )



;; Gets "package com.eddsteel.project.package.name" from
;; - project src/main/scala/com/eddsteel/project/package/name/File.scala
;; - project src/main/scala/project/package/name/File.scala
;; - project src/main/scala/package/name/File.scala
;; - project src/com/eddsteel/project/package/name/File.scala
;; - project src/project/package/name/File.scala
;; - project src/package/name/File.scala
;;
;; Gets "package com.eddsteel.project" from
;; - project src/main/scala/File.scala
;; - project src/File.scala
;;
(defun edd-scala/guess-package (project relfile)
    (let*
      ((root edd-scala/root-package)
       (projectdir (file-name-as-directory project))
       (rootpkgdir (file-name-as-directory (edd-scala/fileize-pkg root)))
       (srcfile (s-chop-prefixes (list "src/" "main/scala/" rootpkgdir projectdir) relfile))
       (srcdir (if-let ((dir (file-name-directory srcfile)))
		   (directory-file-name dir))))
      (concat "package " root "." project (if srcdir (concat "." (edd-scala/pkgize-file srcdir)) ""))))

(defun edd-scala/guess-package-buffer ()
  "Guess and insert the package for this scala file."
  (interactive)
  (if (projectile-project-p)
      (insert (edd-scala/guess-package
	       (file-name-nondirectory (directory-file-name (projectile-project-root)))
	       (car (projectile-make-relative-to-root (list (buffer-file-name))))))
    (message "Must be in a projectile project.")))

(provide 'edd-scala)
