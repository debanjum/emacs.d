;; Setup Package Managers
(require 'package)
(setq package-archives '())
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/") t)
;(package-initialize)  ;; only uncomment for new install

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; ---------------
;; Basic Config
;; ---------------

;; Disable menu, toolbar, scrollbar
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)

;; Replace/Remove selection if present on yank/delete
(delete-selection-mode)

;; show columns by default
(column-number-mode)

;; jump over camelCase words correctly
(global-subword-mode)

;; emacs buffer, window persistence
(setq desktop-save-mode t
      desktop-path '("."))

;; Emacs thinks a sentence a full-stop followed by 2 spaces. Let’s make it full-stop and 1 space.
(setq sentence-end-double-space nil)

;; Set current buffer name in emacs X11 window title
(setq frame-title-format "%b - Emacs")

;; Answer with y or n instead of the default yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Inhibit startup screen
(setq inhibit-startup-screen t)

;; Blank *scratch* buffer
(setq initial-scratch-message nil)

;; Garbage Collection after 20MB
(setq gc-cons-threshold (* 20 1024 1024))

;; Make clipboard work well with X applications
(setq select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; No newlines past EOF
(setq next-line-add-newlines nil)

;; Tramp default ssh
(setq tramp-default-method "ssh")

;; C spacing = 4 instead of default 2
(setq-default c-basic-offset 4)

;; enable some commands that are disabled for dummies
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; backup in single, flat directory
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;; forward, previous paragraph
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;; next, previous buffer
(global-set-key (kbd "C-c C-p") 'previous-buffer)
(global-set-key (kbd "C-c C-n") 'next-buffer)

;; Use UTF-8
(prefer-coding-system 'utf-8)

;; Ansi color shell output
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Kill-region for an active (transient-mark-mode) region but backward-kill-word otherwise
(defun backward-kill-word-or-kill-region (&optional arg)
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))
(global-set-key (kbd "C-w") 'backward-kill-word-or-kill-region)

;; Fullscreen
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value) 'fullboth)))))
(global-set-key [f11] 'toggle-fullscreen)

;; Global Default Zoom for Work
(global-set-key (kbd "C-x C-g +") '(lambda () (interactive) (set-face-attribute 'default nil :height 180)))
(global-set-key (kbd "C-x C-g -") '(lambda () (interactive) (set-face-attribute 'default nil :height 130)))


;; ---------------
;; Tools
;; ---------------
(use-package diminish
  :ensure t
  :diminish auto-revert-mode
  :diminish undo-tree-mode
  :diminish which-key-mode
  :diminish subword-mode
  :diminish eldoc-mode)

(use-package dired
  :bind (:map dired-mode-map
              ("C-c C-e" . wdired-change-to-wdired-mode))
  :init
  (setq dired-dwim-target t
        dired-recursive-copies 'top
        dired-recursive-deletes 'top
        dired-listing-switches "-alh")
  :hook (dired-mode . dired-hide-details-mode))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init (which-key-mode))
(use-package browse-kill-ring
  :ensure t
  :config (browse-kill-ring-default-keybindings))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config (progn (global-undo-tree-mode 1)
                 (setq undo-tree-visualizer-timestamps t)
                 (setq undo-tree-visualizer-diff t)))


;; Uniquify buffer names foo.c:src, foo.c:extra
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse uniquify-separator ":")

;; no tabs by default. modes that really need tabs should enable
;; indent-tabs-mode explicitly. e.g makefile-mode already does that
(setq-default indent-tabs-mode nil)

;; whitespace-cleanup-mode. remove whitespaces on buffer save
(use-package whitespace-cleanup-mode
  :ensure t
  :hook ((python-mode . whitespace-cleanup-mode)
         (org-mode . whitespace-cleanup-mode))
  :init (add-hook
           'write-file-functions
           (lambda () (untabify (point-min) (point-max)) nil)))

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
              ido-everywhere t
              ido-use-virtual-buffers t)))

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
              (find-file (ido-completing-read "Find recent file: " recentf-list)))))

;; ag - the silver searcher
(use-package ag
  :ensure t
  :commands (ag ag-regexp ag-project)
  :config (setq ag-highlight-search t))

;; Magit for Git
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; Add user elisp load path
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Zeitgiest Integration
(use-package zeitgeist)  ;; not portable, but doesn't block/fail emacs load

;; Setup Mail: mu4e, smtpmail
(use-package setup-mail)

;; Company mode for Completion
(use-package company :ensure t :defer t :diminish company-mode)

;; Custom Beancount Company backend
;(use-package company-ledger :load-path "~/.emacs.d/lisp/company-ledger.el")
;(add-to-list 'completion-at-point-functions 'transaction-completion-at-point)
(use-package company-ledger
  :ensure company
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-ledger-backend)))

;; Beancount Minor Mode
;; Get beancount.el from https://bitbucket.org/blais/beancount
(use-package beancount
  :hook (beancount-mode . company-mode)
  :mode ("\\.bean\\'" . beancount-mode))

;; Nov.el Epub Reader Mode
(use-package nov
  :ensure t
  :ensure xml+
  :mode ("\\.epub\\'" . nov-mode))

(use-package org-randomnote
  :ensure t
  :bind ("C-c r" . org-randomnote)
  :config (setq org-randomnote-candidates '("~/Notes/Schedule.org" "~/Notes/Incoming.org" "~/Notes/Archive.org" "~/Notes/Bucket.org")))

;; Drag and drop images/files to attach to org task
(use-package org-download
  :ensure t
  :config
  ;; add support to dired
  (add-hook 'dired-mode-hook 'org-download-enable)
  (setq org-download-method 'attach))

(use-package all-the-icons :ensure t) ;; icon set

;; ---------------
;; Major Packages
;; ---------------

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
             org-log-into-drawer t
             org-reverse-note-order t

             ;; Speed Commands
             org-use-speed-commands t

             ;; Enforce TODO dependency chains
             org-enforce-todo-dependencies t

             ;; Don't dim blocked tasks. Speeds up agenda generation and don't need it always on globally
             org-agenda-dim-blocked-tasks nil

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

             ;; Attachments
             org-attach-directory (concat org-directory "data")
             org-attach-expand-link 'FILE

             ;; Org Default File App to Open
             org-file-apps '(("\\.mm\\'" . default)
                             ("\\.x?html?\\'" . "firefox %s")
                             ("\\.pdf\\'" . default)
                             (auto-mode . emacs))

             ;; Org-Babel execute without confirmation
             org-confirm-babel-evaluate nil

             ;; Org-Mode Link Search
             org-link-search-must-match-exact-headline nil

             ;; Default to Boolean Search
             org-agenda-search-view-always-boolean t
             org-agenda-text-search-extra-files (list "~/Notes/Incoming.org" "~/Notes/Archive.org")

             org-publish-project-alist
             `(
               ("work"
                :base-directory "~/Notes/"
                :base-extension "org"
                :publishing-directory "~/Public/Workstation/"
                :exclude ".*"
                :include ("Schedule.org")
                :exclude-tags ("DATALAD" "CEERI" "HH" "PERSONAL" "TRAVEL" "JobStudy")
                :select-tags ("WORK")
                :with-timestamps t
                :with-tasks t
                :with-tags t
                :with-tables t
                :with-priority t
                :with-planning t
                :publishing-function org-org-publish-to-org
                )
               )

             ;; Include Org Modules
             org-modules '(org-habit org-drill org-tempo)
             org-drill-add-random-noise-to-intervals-p t ;; add random noise to repeat interval of org-drill

             ; Org-Habit Settings
             org-habit-preceding-days 30
             org-habit-following-days 3
             org-habit-graph-column 80

             ;; Custom Agenda's:
             org-agenda-custom-commands
             '(("p" "Play Music" search ""                     ;; this triggers search in given restricted file, but need to pass search term
                ((org-agenda-files '("~/Notes/Music.org"))
                 (org-agenda-text-search-extra-files nil)))
               ;;("w" tags-tree "+WORK-DATALAD-CEERI-HH-JobStudy-PERSONAL-TRAVEL")
               ;;("W" tags-tree "WORKITEM")
               )

             ;; Set custom faces for categories in agenda
             org-agenda-category-icon-alist `(("Work" ,(list (all-the-icons-faicon "cogs")) nil nil :ascent center)
                                              ("Habit" ,(list (all-the-icons-faicon "circle-o-notch")) nil nil :ascent center)
                                              ("" ,(list (all-the-icons-faicon "clock-o")) nil nil :ascent center))

             ;; Set Effort Estimates, Column View
             org-global-properties (quote (("Effort_ALL" . "0:15 0:30 1:00 2:00 4:00 8:00 16:00")))
             org-columns-default-format "%80ITEM(Task) %TAGS(Context) %7TODO(State) %10Effort(Estim){:} %10CLOCKSUM(Clock)"

             ;; Set Tags, Tag Groups and Columns Width
             org-tags-column -78
             org-tag-alist '((:startgroup . nil) ("@WORK" . ?o) ("@HOME" . ?m) ("@COMMUTE" . ?c) (:endgroup . nil) ; { @WORK(o) @HOME(m) @COMMUTE(c) }
                             (:startgroup . nil) ("HACK" . ?h) ("UNDERSTAND" . ?u) ("EXPERIENCE" . ?e) (:endgroup . nil) ; { HACK(h) UNDERSTAND(u) EXPERIENCE(e) }
                             (:startgroup . nil) ("TRY" . ?t) ("MAINTAIN" . ?n) ("FIX" . ?x) ("UPGRADE" . ?r) (:endgroup . nil) ; { TRY(t) MAINTAIN(n) FIX(x) UPGRADE(r) }
                             (:startgroup . nil) ("PERSONAL" . ?p) ("SOCIAL" . ?s) ("WORK" . ?w) ("TOOLS" . ?g) (:endgroup . nil) ;{ PERSONAL(p) SOCIAL(s) WORK(w) TOOLS(g)}
                             ("CALL" . ?a) ("BUY" . ?y) ("IDLE" . ?d) ("HEALTH" . ?l) ("FINANCE" . ?f) ("NOTE" . ?j)) ; CALL(a) BUY(y) IDLE(d) HEALTH(l) FINANCE(f) NOTE(j)

             ;; Customise Refile (C-c C-w)
             org-refile-use-outline-path 'file ;; specify in file.org/heading/sub-heading format
             org-outline-path-complete-in-steps t ;; use TAB for completion
             org-refile-targets '((nil :maxlevel . 4) ;; refile-target = depth 4 in agenda files
                                  (org-agenda-files :maxlevel . 4))

             ;; Setup Org Capture
             org-default-notes-file (concat org-directory "Schedule.org")
             org-capture-templates '(("s" "Schedule" entry (file+headline "Schedule.org" "SCHEDULE")
                                      "** TODO %^{Plan} %^g\n%?\n" :prepend t :kill-buffer t :empty-lines 1)

                                     ;; Ask For Heading, then TAGS, then let user edit entry
                                     ("i" "Incoming" entry (file+headline "Incoming.org" "INCOMING")
                                      "** %?\n   CAPTURED: %U\n  LOCATION: [[file:%F::%i][filelink]] | %a\n" :prepend t :kill-buffer t)

                                     ;; Ask For Heading, then TAGS, then let user edit entry
                                     ("n" "Note" entry (file+headline "Incoming.org" "INCOMING")
                                      "** %U\n   %?" :prepend t :kill-buffer t)

                                     ;; Jumps to clocked in entry
                                     ("a" "Append to Clocked" item (clock) "\t%i %?")

                                     ;; For Web/Mail Capture
                                     ("m" "Mail" entry (file+headline "Schedule.org" "SCHEDULE")
                                      "** TODO \n   CAPTURED: %U\n   LOCATION: %?\n" :prepend t :empty-lines 1)

                                     ;; Create Work Entry with :Work: tag. Note capture time, location
                                     ("w" "Work" entry (file+headline "Schedule.org" "SCHEDULE")
                                      "** TODO %^{Title} :WORK:%^G\n   CAPTURED: %U\n   LOCATION: [[file:%F::%i][filelink]] | %a\n   %?"
                                      :prepend t :kill-buffer t :empty-lines 1)

                                     ;; Create Meeting Entry with :Call: tag. Note capture time, people, meeting location
                                     ("c" "Meeting" entry (file+headline "Schedule.org" "SCHEDULE")
                                      "** TODO %^{Title} :CALL:%^G\n   CAPTURED: %U\n   LOCATION: %^{Where?}\n   PEOPLE: %^{Who?}\n   %?"
                                      :prepend t :empty-lines 1)))

            ;; Org-Babel tangle
            (require 'ob-tangle)

            ;; Setup Babel languages. Can now do Literate Programming
            (org-babel-do-load-languages 'org-babel-load-languages
                                         '((python . t)
                                           (shell . t)
                                           (emacs-lisp . t)
                                           (ledger . t)
                                           (ditaa . t)
                                           (js . t)
                                           (C . t)))
            ;; Org formatted clipboard URL formatted with Title from Webpage
            (use-package org-cliplink
              :ensure t
              :bind ("C-x p i" . 'org-cliplink))

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
                                          ("osm"    . "https://nominatim.openstreetmap.org/search?q=%s&polygon=1")
                                          ("vso"    . "https://o365exchange.visualstudio.com/O365%20Core/_git/Griffin/pullrequest/%s")
                                          ("attach" . org-attach-expand-link)
                                          ("transaction" . "file:Ledger.bean::%s")))

            ;; Thunderlink. Open an email in Thunderbird with ThunderLink.
            (defun org-thunderlink-open (path) (start-process "myname" nil "thunderbird" "-thunderlink" (concat "thunderlink:" path)))
            (org-link-set-parameters
             "thunderlink"
             :follow 'org-thunderlink-open
             :face '(:foreground "dodgerblue" :underline t))

            (org-link-set-parameters
             "mu4e"
             :face '(:foreground "dodgerblue" :underline t))

            (org-link-set-parameters
             "C"
             :follow '(lambda(path) (message "Link only available on Windows"))
             :face '(:foreground "darkgoldenrod" :underline t :strike-through t))

            (org-link-set-parameters
             "E"
             :follow '(lambda(path) (message "Link only available on Windows"))
             :face '(:foreground "darkgoldenrod" :underline t :strike-through t))

            (org-link-set-parameters
             "outlook"
             :follow '(lambda(path) (message "Outlook not available on linux"))
             :face '(:foreground "dodgerblue" :underline t :strike-through t)
             :help-echo "Outlook not available on this machine")

            ;;store org-mode links to messages
            (require 'org-mu4e)
            ;;store link to message if in header view, not to header query
            (setq org-mu4e-link-query-in-headers-mode nil)

            (require 'org-contacts)
            (setq org-contacts-files (list "/home/linux/Notes/Contacts.org"))

            ;;task state dependency chaining
            (require 'org-depend)


            (defun deb/upsert-org-entry-ids ()
              "Add/update ID of all visible entry. Useful after copying subtree to prevent duplicate ids"
              (interactive)
              (org-map-entries '(lambda () (org-id-get-create t))))

;; Org-Music Mode
(use-package org-music
  :load-path "~/.emacs.d/lisp/org-music.el"
  :init (progn
	  (add-hook
	   'org-mode-hook
	   (lambda()
	     (if (equal buffer-file-name (expand-file-name "~/Notes/Music.org"))
		 (org-music-mode))))))

;; Set SBCL as default lisp interpreter
(if (executable-find "sbcl") (setq inferior-lisp-program (executable-find "sbcl")))

;; Elpy for Python
(use-package elpy
  :ensure t
  :commands elpy-enable
  :init (with-eval-after-load 'python (elpy-enable))
  :config (progn
            (setq
             elpy-test-nose-runner-command '("nosetests" "-s" "-v")
             elpy-test-runner 'elpy-test-nose-runner)))

;; Make ipython 5.x (color)compatible with Emacs eshell
(if (executable-find "ipython") (setq python-shell-interpreter "ipython" python-shell-interpreter-args "--simple-prompt -i"))

;; Realgud Enhanced Debugging
;(use-package realgud :ensure t :defer t)
;(with-eval-after-load 'python (progn
;                               (load "realgud")
;                               (define-key python-mode-map (kbd "C-c g") 'realgud:pdb)
;                               (use-package epdb :load-path "~/.emacs.d/lisp")))
;;; EPDB Integration
;(use-package epdb :load-path "~/.emacs.d/lisp/")  ;; not portable, but doesn't block/fail emacs load

;; Intero for Haskell
(use-package intero
  :ensure t
  :defer t
  :hook (haskell-mode . intero-mode))

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
  :hook ((flycheck-mode . js2-mode)
         (tern-mode .js2-mode)))

;; Tern for Javascript
(use-package tern :ensure t :defer t)
(use-package tern-auto-complete
  :ensure t
  :commands tern-ac-setup
  :init (with-eval-after-load 'tern (tern-ac-setup)))

;; Set Xelatex as default latex(C-c C-c), if Xelatex installed on system
(if (executable-find "xelatex") (setq latex-run-command "xelatex"))
(defun my-latex-setup ()
  (defun latex-word-count ()
    (interactive)
    (let* ((this-file (buffer-file-name))
           (word-count
            (with-output-to-string
              (with-current-buffer standard-output
                (call-process "texcount" nil t nil "-brief" this-file)))))
      (string-match "\n$" word-count)
      (message (replace-match "" nil nil word-count))))
    (define-key latex-mode-map "\C-cw" 'latex-word-count))
(add-hook 'latex-mode-hook 'my-latex-setup t)

;; Markdown Mode
(use-package markdown-mode
  :ensure t
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Organise feeds in an Org file
(use-package elfeed-org
  :ensure t
  :config (progn
            (elfeed-org)
            (setq rmh-elfeed-org-files (list "~/Notes/Feed.org"))))

;; elfeed helper functions
(defun elfeed-load ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force))
(defun deb/elfeed-save-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))
(defun deb/elfeed-frontpage ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-frontpage"))
(defun deb/elfeed-all ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-all"))
(defun deb/elfeed-understand ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-understand"))
(defun deb/elfeed-experience ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-experience"))
(defun deb/elfeed-hack ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-hack"))

(use-package elfeed
  :ensure t
  :defer t
  :init (setq elfeed-db-directory "~/Notes/.elfeed")
  :bind (:map elfeed-search-mode-map
	      ("h" . deb/elfeed-hack)
	      ("u" . deb/elfeed-understand)
	      ("e" . deb/elfeed-experience)
	      ("f" . deb/elfeed-frontpage)
	      ("A" . deb/elfeed-all)
              ("q" . deb/elfeed-save-bury)))

(use-package elfeed-web :ensure t)

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic-fallback ("%PDF" . pdf-view-mode)
  :config (pdf-tools-install))

;; ---------------
;; Theme
;; ---------------

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
