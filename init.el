;; Setup Package Managers
(require 'package)
(setq package-archives '())
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/") t)
(package-initialize)

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Org-Mode
(use-package org
  ;; Install package org-plus-contrib for org
  :ensure org-plus-contrib
  ;; Install from 'org' package archive
  :pin org
  ;; Load org in org-mode
  :mode (("\\.org$" . org-mode))
  ;; Bind keys
  :bind (("C-c c" . org-capture)
	 ("C-c l" . org-store-link)
	 ("C-c a" . org-agenda))
  ;; Configure org
  :config (progn
	    (setq
	     org-log-done t
	     org-reverse-note-order t	 

	     ;; Enforce TODO dependency chains
	     org-enforce-todo-dependencies t

	     ;; Syntax hilighting of code blocks in Org-Babel
	     org-src-fontify-natively t
	     ;; Auto-indent of code blocks in Org-Babel
	     org-src-tab-acts-natively t

	     ;; Set Org-Files for Agenda
	     org-directory "~/Notes/"
	     org-agenda-files (list (concat org-directory "Schedule.org"))
	     org-archive-location (concat org-directory "Archive.org::* %s")
	 
	     ;; Mobile Org Settings
	     org-mobile-directory "~/Dropbox/Notes/"
	     org-mobile-files (list "Schedule.org" "Incoming.org")
	     org-mobile-inbox-for-pull (concat org-directory "Schedule.org")
	     
	     ;; Org-Babel execute without confirmation
	     org-confirm-babel-evaluate nil
	     
	     ;; Org-Mode Link Search
	     org-link-search-must-match-exact-headline nil

	     ;; Force UTF-8
	     org-export-coding-system 'utf-8
	     
	     ;; Customise Refile (C-c C-w) 
	     org-refile-use-outline-path 'file         ;; specify in file.org/heading/sub-heading format 
	     org-outline-path-complete-in-steps t      ;; use TAB for completion
	     org-refile-targets '((nil :maxlevel . 6)  ;; refile-target = depth 6 in agenda files
				  (org-agenda-files :maxlevel . 6))
	     
	     ;; Setup Org Capture
	     org-default-notes-file (concat org-directory "Schedule.org")
	     org-capture-templates '(("s" "Schedule" entry (file+headline (concat org-directory "Schedule.org") "SCHEDULE")
				      "** TODO %^{Plan} %^g\n%?\n" :prepend t :kill-buffer t :empty-lines 1)
				     
				     ;; Ask For Heading, then TAGS, then let user edit entry
				     ("i" "Incoming" entry (file+headline (concat org-directory "Incoming.org") "INCOMING")
				      "** %?\n  CAPTURED: %U\n  LOCATION: [[file:%F::%i][filelink]] | %a\n" :prepend t :kill-buffer t)
				     
				     ;; Jumps to clocked in entry
				     ("a" "Append to Clocked" item (clock) "\t%i %?")

				     ;; For Web/Mail Capture
				     ("m" "Mail" entry (file+headline (concat org-directory "Schedule.org") "SCHEDULE")
				      "** TODO \n   CAPTURED: %U\n   LOCATION: %?\n" :prepend t :empty-lines 1)
				     
				     ;; Create Work Entry under Datalad Subheading. Note capture, location 
				     ("w" "Work" entry (file+olp (concat org-directory "Schedule.org")
								 "SCHEDULE" "RECURRING" "Fall 2016" "Datalad")
				      "***** TODO %^{Title}\n      CAPTURED: %U\n      LOCATION: [[file:%F::%i][filelink]] | %a\n      %?"
				      :prepend t :kill-buffer t :empty-lines 1)))

	    ;; Org-Babel tangle
	    (require 'ob-tangle)

	    ;; Setup Babel languages. Can now do Literate Programming
	    (org-babel-do-load-languages 'org-babel-load-languages
					 '((python . t)
					   (sh . t)
					   (emacs-lisp . t)
					   (js . t)
					   (C . t)))
	    
	    ;; Link to specific (git) versions of a file. 
	    (require 'org-git-link)
	    
	    ;; Github Link Formatter
	    (defun gitlink (tag)
	      "converts github issues, pull requests into valid format"
	      (setq ghsplit (split-string tag "/"))
	      (if (string-match-p (regexp-quote "i#") (car (last ghsplit)))
		  (concat (pop ghsplit) "/" (pop ghsplit) "/issues/" (substring (pop ghsplit) 2 nil))
		(if (string-match-p (regexp-quote "p#") (car (last ghsplit)))
		    (concat (pop ghsplit) "/" (pop ghsplit) "/pull/" (substring (pop ghsplit) 2 nil))
		  (concat "" tag))))
	    
	    ;; Shorthand for links
	    (setq org-link-abbrev-alist '(("github" . "https://github.com/%(gitlink)")
					  ("gitlab" . "https://gitlab.com/%(gitlink)")
					  ("google" . "https://www.google.com/search?q=%s")
					  ("gmap"   . "https://maps.google.com/maps?q=%s")
					  ("osm"    . "https://nominatim.openstreetmap.org/search?q=%s&polygon=1")))
	    
	    ;; Thunderlink. Open an email in Thunderbird with ThunderLink.
	    (defun org-thunderlink-open (path) (start-process "myname" nil "thunderbird" "-thunderlink" (concat "thunderlink:" path)))
	    (org-add-link-type "thunderlink" 'org-thunderlink-open)))

;; Zeitgiest Integration
(use-package zeitgeist :load-path "~/.emacs.d/lisp/")  ;; not portable, but doesn't block/fail emacs load

;; Integrate clipboard with x11, if xclip installed on system
(use-package xclip
  :ensure t
  :if (executable-find "xclip")
  :init (xclip-mode 1)
  :config (put 'narrow-to-region 'disabled nil))

;; whitespace-cleanup-mode. remove whitespaces on buffer save
(use-package whitespace-cleanup-mode
  :ensure t
  :init (add-hook 'python-mode-hook #'whitespace-cleanup-mode))

;; Expand-Region for intelligent highlight expansion
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; Ido for file/buffer/../ auto-completion, fuzzy-matching etc
(use-package ido
  :ensure t
  :init (ido-mode 1)
  :config  (progn
	     (setq
	      ido-enable-flex-matching t
	      ido-everywhere t)))

;; Smex for M-x auto-completion, fuzzy-matching etc
(use-package smex
  :ensure t
  :bind (("M-x" . smex))
  :config (smex-initialize))

;; Recentf to suggest recently opened files on C-x C-r
(use-package recentf
  :ensure t
  :init (recentf-mode t)
  :bind ("C-x C-r" . ido-recentf-open)  ;; replace `find-file-read-only` with more a useful command
  :config (progn
	    (setq recentf-max-saved-items 50)
	    (defun ido-recentf-open ()
	      "Use `ido-completing-read` to `find-file` a recent file"
	      (interactive)
	      (if (find-file (ido-completing-read "Find recent file: " recentf-list))
		  (message "Opening file...")
		(message "Aborting")))))

;; Magit for Git
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; Elpy for Python
(use-package elpy
  :ensure t
  :commands elpy-enable
  :init (with-eval-after-load 'python (elpy-enable))
  :config (progn
	    (setq
	     elpy-test-nose-runner-command '("nosetests" "-s" "-v")
	     elpy-test-runner 'elpy-test-nose-runner
	     )))

;; Intero for Haskell
(use-package intero
  :ensure t
  :init (add-hook 'haskell-mode-hook #'intero-mode))

;; Web-Mode for HTML
(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" . web-mode)
  :config (progn
            (add-hook 'web-mode-hook
                      (lambda ()
                        (setq web-mode-enable-css-colorization t
			      web-mode-markup-indent-offset 2
			      web-mode-css-indent-offset 2
			      web-mode-code-indent-offset 2
			      web-mode-style-padding 2
			      web-mode-script-padding 2
			      web-mode-ac-sources-alist
			      '(("css" . (ac-source-css-property))
				("html" . (ac-source-words-in-buffer ac-source-abbrev))))))))

;; Js2-Mode for Javascript
(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.json\\'" . javascript-mode))
  :interpreter ("node" . js2-mode)
  :commands js2-mode
  :config (progn
	    (setq-default js2-basic-offset 2
			  js2-indent-switch-body t
			  js2-auto-indent-p t
			  flycheck-disabled-checkers '(javascript-jshint)
			  flycheck-checkers '(javascript-eslint)
			  flycheck-eslintrc "~/.eslintrc"))
  (add-to-list 'js2-mode-hook 'flycheck-mode 'tern-mode))

;; Tern for Javascript
(use-package tern :ensure t)
(use-package tern-auto-complete
  :ensure t
  :commands tern-ac-setup
  :init (with-eval-after-load 'tern (tern-ac-setup)))


(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode))
  :config (progn
	    (add-hook 'yaml-mode-hook
		      (lambda ()
			(define-key yaml-mode-map "\C-m" 'newline-and-indent)))))

;; Disable toolbar
(tool-bar-mode -1)

;; Copy-Paste from/to Terminal Emacs
(delete-selection-mode)

;; Set current buffer name in emacs X11 window title
(setq frame-title-format "%b - emacs")

;; Answer with y or n instead of the default yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Set Clisp as default lisp interpreter (Slime)
(if (executable-find "sbcl") (setq inferior-lisp-program (executable-find "sbcl")))

;; Set Xelatex as default latex(C-c C-c), if Xelatex installed on system
(if (executable-find "xelatex") (setq latex-run-command "xelatex"))

;; C spacing = 4 instead of default 2
(setq-default c-basic-offset 4)

;; Tramp default ssh
(setq tramp-default-method "ssh")

;; Make ipython 5.x (color)compatible with Emacs eshell
(setq python-shell-interpreter "ipython" python-shell-interpreter-args "--simple-prompt -i")

;; Solarized Emacs Theme @ https://github.com/bbatsov/solarized-emacs
;; If theme not loaded => init.el (partial) failed
(use-package solarized-theme
  :ensure t
  :init (progn
	  ;; Don't change size of org-mode headlines (but keep other size-changes)
	  (setq solarized-scale-org-headlines nil)
	  ;; Don't change the font for some headings and titles
	  (setq solarized-use-variable-pitch nil))
  :config (progn
	    (load "solarized-theme-autoloads" nil t)
	    (load-theme 'solarized-light t)))

;; Spaceline Modeline
(use-package spaceline-config
  :ensure spaceline
  :config (spaceline-emacs-theme))

;; Realgud Enhanced Debugging
(use-package realgud :ensure t)

;; EPDB Integration
(use-package epdb :load-path "~/.emacs.d/lisp/")  ;; not portable, but doesn't block/fail emacs load