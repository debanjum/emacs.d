;; Setup Package Managers
(require 'package)

(setq package-archives '())
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(package-initialize)

;; Install use-package
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Install Quelpa
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

;; Install Quelpa Use-Package
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

;; ---------------
;; Basic Config
;; ---------------

;; Disable menu, toolbar, scrollbar
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)

(use-package exec-path-from-shell
  :ensure t
  :config (when (memq window-system '(mac ns x))
            (exec-path-from-shell-initialize)))

;; Replace/Remove selection if present on yank/delete
(delete-selection-mode)

;; show columns by default
(column-number-mode)

;; jump over camelCase words correctly
(global-subword-mode)

;; emacs buffer, window persistence
(setq desktop-save-mode 1
      desktop-path '("."))

;; as long as packages depend on cl instead of cl-lib this warning will remain
(setq byte-compile-warnings '(cl-functions)) ;

;; save command executed history
(setq
 savehist-additional-variables '(counsel-M-x-history command-history extended-command-history)
 savehist-mode 1)

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

;; All prompts in minibuffer instead of in GUI popup
(setq use-dialog-box nil)

;; Garbage Collection after 20MB
(setq gc-cons-threshold (* 20 1024 1024))

;; Make clipboard work well with MacOS Clipboard
(defun copy-from-macos ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-macos (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(when (memq window-system '(mac ns x))
  (progn
    ;;;;; Clipboard Mac OS X
    (setq interprogram-cut-function 'paste-to-macos)
    (setq interprogram-paste-function 'copy-from-macos)))

; To show emoji's on MacOS by using Apple Color Emoji font for symbols
(when (memq window-system '(mac ns x))
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))

;; No newlines past EOF
(setq next-line-add-newlines nil)

;; Tramp default ssh
(setq tramp-default-method "ssh")

;; C spacing = 4 instead of default 2
(setq-default c-basic-offset 4)

;; enable some advanced commands
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

;; M-w to yank line if no active region selected
(defun yank-line (beg end)
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Yanked current line")
     (list (line-beginning-position) (line-beginning-position 2)))))

(advice-add 'kill-ring-save :before #'yank-line)

(defun yank-replace-rectangle (start end)
  "Similar to yank-rectangle, but deletes selected rectangle first."
  (interactive "r")
  (delete-rectangle start end)
  (pop-to-mark-command)
  (yank-rectangle))
(global-set-key (kbd "C-x r C-y") 'yank-replace-rectangle)

;; Fullscreen
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value) 'fullboth)))))
(global-set-key [f11] 'toggle-fullscreen)

;; Global Default Zoom for Work Monitors
(global-set-key (kbd "C-x C-g +") '(lambda () (interactive) (set-face-attribute 'default nil :height 180)))
(global-set-key (kbd "C-x C-g -") '(lambda () (interactive) (set-face-attribute 'default nil :height 130)))

;; Puts custom-set-variables into a separate temporary file
(setq custom-file (make-temp-file "emacs-custom"))

;; Spell Check
(when (executable-find "aspell")
  (setq-default ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "--camel-case")))

;; ---------------
;; Tools
;; ---------------
(use-package dired
  :bind (:map dired-mode-map
              ("C-c C-e" . wdired-change-to-wdired-mode))
  :init
  (setq dired-dwim-target t
        dired-recursive-copies 'top
        dired-recursive-deletes 'top
        dired-listing-switches "-alh")
  :hook (dired-mode . dired-hide-details-mode))

;; Jump to first non-whitespace character on line or beginning of line
;; NOTE: crux-move-beginning-of-line has been modified by me.
;;       Jumps to start of line first, jumps to first non-whitespace character if already at start of line
(use-package crux
  :ensure t
  :bind (("C-a" . crux-move-beginning-of-line)))

;; Allow defining keybinding chords via use-package
(use-package use-package-chords
  :ensure t
  :config
  (key-chord-mode 1))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init (which-key-mode))

(use-package avy
  :ensure t
  :init
  (setq avy-keys-alist
        `((avy-goto-char-timer . (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
          (avy-goto-line . (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i)))
        avy-style 'pre)
  :bind* (("M-m f" . avy-goto-char-timer)
          ("M-m F" . avy-goto-line)
          (:map isearch-mode-map ("C-'" . avy-isearch))))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :chords (("uu" . undo-tree-visualize))
  :config (progn (global-undo-tree-mode 1)
                 (setq undo-tree-visualizer-timestamps t)
                 (setq undo-tree-visualizer-diff t)))

(use-package dumb-jump
  :ensure t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
)

;; Uniquify buffer names foo.c:src, foo.c:extra
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse uniquify-separator ":")

;; Cache passphrase for symmetric encrypted files
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

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

;; Faster window switching
(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window))
  :config (progn
            (setq aw-keys '(?a ?s ?k ?l ?d ?j ?h ?f ?g))))

;; Ivy for completion function everywhere where ido isn't used (for now)
(use-package ivy
  :ensure t
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-c C-j" . ivy-immediate-done)) ; default keybinding C-M-j conflicts with an MacOS keybinding
  :config (progn
            (ivy-mode)
            (setq
             ivy-use-virtual-buffers t
             enable-recursive-minibuffers t
             ivy-count-format ""
             ivy-initial-inputs-alist nil
             ivy-sort-matches-functions-alist '((t . nil)) ;; To sort most recent first
             ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                     (t      . ivy--regex-fuzzy)))))

(use-package counsel
  :ensure t
  :after (ivy use-package-chords)
  :diminish
  :chords (("yy" . counsel-yank-pop))
  :config (progn
            (counsel-mode)))

(use-package swiper
  :ensure t
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper-backward)))

;; Amx for M-x persistent MRU for auto-completion. Replaces Smex as that's deprecated.
;; Amx can rely on Ivy via counsel-M-x or ido completion backend for fuzzy candidate matching etc.
(use-package amx
  :ensure t
  :config (amx-mode))

;; Recentf to suggest recently opened files on C-x C-r
(use-package recentf
  :ensure t
  :init (recentf-mode t)
  :bind ("C-x C-r" . counsel-recentf)  ;; replace `find-file-read-only` with more a useful command
  :config (setq recentf-max-saved-items 50))

;; To keep GNU ELPA Keyring up-to-date
(use-package gnu-elpa-keyring-update
  :ensure t)

;; Gnuplot for Plotting in org
(use-package gnuplot :ensure t)

;; ag - the silver searcher
(use-package ag
  :ensure t
  :commands (ag ag-regexp ag-project)
  :config (setq ag-highlight-search t))

;; Magit for Git
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; Magit Find File
(use-package magit-find-file
  :ensure t
  :bind ("C-c p" . magit-find-file-completing-read)
  :config (setq magit-completing-read-function 'ivy-completing-read))

;; Orgit: Support for Org links to Magit buffers
(use-package orgit
  :ensure t)

;; Add user elisp load path
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Zeitgiest Integration
;(use-package zeitgeist)  ;; not portable, but doesn't block/fail emacs load

;; Setup Mail: mu4e, smtpmail
(use-package setup-mail)

;; Company mode for Completion
(use-package company :ensure t :defer t :diminish company-mode)

;; Beancount Company backend
(use-package company-ledger
  :ensure t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-ledger)))

 ;; Trigger completion immediately.
(setq company-idle-delay 0)

 ;; Number the candidates (use M-1, M-2 etc to select completions).
(setq company-show-numbers t)

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

;; Annotation for ebooks, pdf's etc in org files
(use-package org-noter
  :ensure t
  :config (progn
            (setq
             org-noter-auto-save-last-location t
             org-noter-notes-search-path '("~/Notes"))))

(use-package org-randomnote
  :ensure t
  :bind ("C-c r" . org-randomnote)
  :config (setq org-randomnote-candidates '("~/Notes/Schedule.org" "~/Notes/Incoming.org" "~/Notes/Archive.org")))

(use-package clip2org :config (setq clip2org-clippings-file (expand-file-name "~/Documents/eBooks/My Clippings.txt")))

;; Drag and drop images/files to attach to org task
(use-package org-download
  :ensure t
  :config
  ;; add support to dired
  (add-hook 'dired-mode-hook 'org-download-enable)
  (setq org-download-screenshot-method "/usr/sbin/screencapture -s %s")
  (setq org-download-method 'attach))

(use-package all-the-icons :ensure t) ;; icon set

(use-package flycheck
  :ensure t
  :config (setq flycheck-emacs-lisp-load-path 'inherit))

(use-package ruby-mode
  :ensure t
  :mode "\\.rb\\'")

;(use-package sonic-pi
;  :ensure t
;  :config
;  (add-hook
;   'sonic-pi-mode-hook
;   (lambda ()
;     ;; This setq can go here instead if you wish
;     (setq sonic-pi-path "/Applications/Sonic\ Pi.app/")
;     (setq sonic-pi-server-bin "Contents/Resources/app/server/ruby/bin/sonic-pi-server.rb"))))

(use-package plantuml-mode
  :after org-mode
  :ensure t
  :config
  (progn
    (setq
     org-plantuml-jar-path
     (expand-file-name "~/Builds/PlantUML/plantuml.1.2020.9.jar"))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))))


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
  ;; Configure before loading org.el
  :init (progn
          (setq
           ;; Export backends. Needs to be set before org.el is loaded
           org-export-backends '(ascii html icalendar latex md org odt)
           org-export-coding-system 'utf-8
           org-export-babel-evaluate nil
           org-export-with-sub-superscripts nil  ; _, ^ aren't exported to sub/superscript

           ;; Org-Calendar Export
           org-icalendar-combined-agenda-file (expand-file-name "~/Sync/Phone/agenda.ics")
           org-icalendar-combined-name "D's Org Agenda"
           org-icalendar-use-scheduled '(todo-start event-if-todo)
           org-icalendar-use-deadline '(todo-due event-if-todo)
           org-icalendar-store-UID t
           org-icalendar-include-todo 'all
           ))

  ;; Configure org
  :config (progn
            (setq
             org-log-done t
             org-log-into-drawer t
             org-reverse-note-order t

             ;; Speed Commands
             org-use-speed-commands t

             ;; Allow user specified scaling of org-inline images.
             org-image-actual-width nil

             ;; Allow setting scheduled time if multiple entries selected
             org-loop-over-headlines-in-active-region t

             ;; Prevent invisible edits and show region
             org-catch-invisible-edits 'show-and-error

             ;; Enforce TODO dependency chains
             org-enforce-todo-dependencies t

             ;; Don't dim blocked tasks. Speeds up agenda generation and don't need it always on globally
             org-agenda-dim-blocked-tasks nil

             ;; Allow multiple agenda views to be visible at same time
             ;; Show existing agenda buffers instead of generating new ones every time
             org-agenda-sticky t

             ;; Set Org-Files for Agenda
             org-directory "~/Notes/"
             org-agenda-files (list (concat org-directory "Schedule.org"))
             org-archive-location (concat org-directory "Archive.org::* %s")

             ;; Mobile Org Settings
             org-mobile-directory "~/Dropbox/Notes/"
             org-mobile-files (list "Schedule.org" "Incoming.org")
             org-mobile-inbox-for-pull (concat org-directory "Schedule.org")

             ;; Attachments
             org-attach-id-dir "~/Notes/data/"

             ;; Org-Babel
             ;; set cider as org babel clojure backend
             org-babel-clojure-backend 'cider
             ;; execute without confirmation
             org-confirm-babel-evaluate nil
             ;; Syntax hilighting of code blocks
             org-src-fontify-natively t
             ;; Auto-indent of code blocks
             org-src-tab-acts-natively t

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
             org-modules '(org-habit)

             ; Org-Habit Settings
             org-habit-preceding-days 30
             org-habit-following-days 3
             org-habit-graph-column 80

             ;; Org Agenda To-Do/Tag-Match Ignore Scheduled/Deadlined Tasks
             ;; Allows view of "non-assigned/processed" tasks
             org-agenda-todo-ignore-scheduled 'all
             org-agenda-todo-ignore-deadlines 'all
             org-agenda-tags-todo-honor-ignore-options t

             ;; Custom Agenda's:
             org-agenda-custom-commands
             '(
               ("A" "Immediate Tasks" todo "TODO|ACTIVE|WAITING")
               ("p" "Play Music" search ""                     ;; this triggers search in given restricted file, but need to pass search term
                ((org-agenda-files '("~/Notes/Music.org"))
                 (org-agenda-text-search-extra-files nil))))

             ;; Set custom faces for categories in agenda
             org-agenda-category-icon-alist `(("Work" ,(list (all-the-icons-faicon "cogs")) nil nil :ascent center)
                                              ("Habit" ,(list (all-the-icons-faicon "circle-o-notch")) nil nil :ascent center)
                                              ("Music" ,(list (all-the-icons-faicon "youtube-play")) nil nil :ascent center)
                                              ("" ,(list (all-the-icons-faicon "clock-o")) nil nil :ascent center))

             ;; Set Effort Estimates, Column View
             org-global-properties (quote (("Effort_ALL" . "0:15 0:30 1:00 2:00 4:00 8:00 16:00")))
             org-columns-default-format "%80ITEM(Task) %7TODO(State) %10Effort(Effort){:} %CLOCKSUM(Clocked) %TAGS(Tags)"

             ;; Set Tags, Tag Groups and Columns Width
             org-tags-column -78
             org-tag-alist '((:startgroup) ("@WORK" . ?o) ("@HOME" . ?m) ("@COMMUTE" . ?c) (:endgroup)
                             (:startgroup) ("HACK" . ?h) ("UNDERSTAND" . ?u) ("EXPERIENCE" . ?e) (:endgroup)
                             (:startgroup) ("TRY" . ?t) ("MAINTAIN" . ?n) ("FIX" . ?x) ("UPGRADE" . ?r) (:endgroup)
                             (:startgroup) ("PERSONAL" . ?p) ("ENVIRONMENT" . ?v) ("SOCIAL" . ?s) ("WORK" . ?w) (:endgroup)
                             (:startgroup) ("READ" . ?R) ("WATCH" . ?W) ("LISTEN" . ?L) (:endgroup)
                             ("TOOLS" . ?g) ("CALL" . ?a) ("BUY" . ?y) ("IDLE" . ?d) ("HEALTH" . ?l) ("FINANCE" . ?f) ("NOTE" . ?j))

             ;; Customise Refile (C-c C-w)
             org-refile-use-outline-path 'file ;; specify in file.org/heading/sub-heading format
             org-outline-path-complete-in-steps nil ;; use TAB for completion
             org-refile-use-cache t  ;; use cache for speed-up
             org-refile-targets '((nil :maxlevel . 4) ;; refile-target = depth 4 in agenda files
                                  (org-agenda-files :maxlevel . 4))

             ;; Setup Org Capture
             org-default-notes-file (concat org-directory "Schedule.org")
             org-capture-templates '(("s" "Schedule" entry (file+headline "Schedule.org" "PROJECTS")
                                      "** TODO %^{Plan} %^g\n%?\n" :prepend t :kill-buffer t :empty-lines 1)

                                     ;; Ask For Heading, then TAGS, then let user edit entry
                                     ("i" "Incoming" entry (file+headline "Incoming.org" "RESOURCES")
                                      "** %?\n   CAPTURED: %U\n  LOCATION: [[file:%F::%i][filelink]] | %a\n" :prepend t :kill-buffer t)

                                     ;; Ask For Heading, then TAGS, then let user edit entry
                                     ("n" "Note" entry (file+headline "Incoming.org" "RESOURCES")
                                      "** %U\n   %?" :prepend t :kill-buffer t)

                                     ;; Jumps to clocked in entry
                                     ("a" "Append to Clocked" item (clock) "\t%i %?")

                                     ;; For Web/Mail Capture
                                     ("m" "Mail" entry (file+headline "Schedule.org" "PROJECTS")
                                      "** TODO %^{Title} :WEB:READ:\n   CAPTURED: %U\n   %?" :prepend t :empty-lines 1)

                                     ;; For Web/Mail Capture
                                     ("p" "Plan for Today" entry (id "2f6dbcf9-20f0-4341-8390-e51a987a3e01")
                                      "*** %u: Plan for Today\n    - [%] Major\n      - [ ] %?\n    - [%] Minor\n" :prepend t :empty-lines 1)

                                     ;; Create Work Entry with :Work: tag. Note capture time, location
                                     ("w" "Work" entry (file+headline "Schedule.org" "PROJECTS")
                                      "** TODO %^{Title} :WORK:%^G\n   CAPTURED: %U\n   LOCATION: [[file:%F::%i][filelink]] | %a\n   %?"
                                      :prepend t :kill-buffer t :empty-lines 1)

                                     ;; Create Meeting Entry with :Call: tag. Note capture time, people, meeting location
                                     ("c" "Meeting" entry (file+headline "Schedule.org" "PROJECTS")
                                      "** TODO %^{Title} :CALL:%^G\n   CAPTURED: %U\n   LOCATION: %^{Where?}\n   PEOPLE: %^{Who?}\n   %?"
                                      :prepend t :empty-lines 1)))

            ;; Org-Babel tangle
            (require 'ob-tangle)
            (require 'ob-clojure)
            (require 'cider)

            ;; Setup Babel languages. Can now do Literate Programming
            (org-babel-do-load-languages 'org-babel-load-languages
                                         '((python . t)
                                           (shell . t)
                                           (sudo . t)  ;; ob-sudo. try sudo in babel instead of using :dir "sudo::/path/to/dir"
                                           (emacs-lisp . t)
                                           (ledger . t)
                                           (ditaa . t)
                                           (plantuml . t)
                                           (clojure . t)
                                           (js . t)
                                           (C . t)))
            (use-package ob-tmux
              :ensure t
              :custom
              (org-babel-default-header-args:tmux
               '((:results . "silent")     ;
                 (:session . "default")    ; The default tmux session to send code to
                 (:socket  . nil)          ; The default tmux socket to communicate with
                 (:terminal . "zsh")))
              ;; The tmux sessions are prefixed with the following string.
              ;; You can customize this if you like.
              (org-babel-tmux-session-prefix ""))

            ;; Org formatted clipboard URL formatted with Title from Webpage
            (use-package org-cliplink
              :ensure t
              :bind ("C-x p i" . 'org-cliplink))

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

             ;; Refresh org-refile cache on Emacs idle > 5 mins
             (run-with-idle-timer 900 t (lambda ()
                                          (org-refile-cache-clear)
                                          (org-refile-get-targets)))

            (defun d/org-id-complete-link (&optional arg)
              "Create an id: link using completion"
              (concat "id:" (org-id-get-with-outline-path-completion
                             '((nil :maxlevel . 4)
                               (org-agenda-files :maxlevel . 4)))))

            (defun d/org-id-complete-link-description (link desc)
              (if (and (string-prefix-p "id:" link)
                       (or (not desc) (string= "" desc)))
                  (let* ((id (string-remove-prefix "id:" link)))
                    (save-excursion
                      (org-id-goto id)
                      (nth 4 (org-heading-components))))
                desc))

            ;; Store id based link to org entry at point via C-c l
            (setq org-id-link-to-org-use-id t)

            ;; Set org entry heading/title as default description for id:<id> type links
            (setq org-link-make-description-function #'d/org-id-complete-link-description)

            (org-link-set-parameters
             "id"
             :complete 'd/org-id-complete-link)

            ;;store org-mode links to messages
            (require 'org-mu4e)
            ;;store link to query if in header view, not the message cursor is currently on
            (setq org-mu4e-link-query-in-headers-mode t)

            (require 'org-contacts)
            (setq org-contacts-files (list (expand-file-name "~/Notes/Contacts.org")))

            ;;task state dependency chaining
            (require 'org-depend)

            (use-package org-mime :ensure t)
            (defun htmlize-and-send ()
              "When in an org-mu4e-compose-org-mode message, htmlize and send it."
              (interactive)
              (when (member 'org~mu4e-mime-switch-headers-or-body post-command-hook)
                (org-mime-htmlize)
                (message-send-and-exit)))
            (add-hook 'org-ctrl-c-ctrl-c-hook 'htmlize-and-send t)

            (defun deb/upsert-org-entry-ids ()
              "Add/update ID of all visible entry. Useful after copying subtree to prevent duplicate ids"
              (interactive)
              (org-map-entries '(lambda () (org-id-get-create t))))
            ))

(use-package org-drill
  :ensure t
  :config (progn
            (setq org-drill-add-random-noise-to-intervals-p t) ; add random noise to repeat interval

            ;; Quickstart for org drill
            ;; Primarily used for Spaced Repetition habit on Phone (via Tasker, Termux)
            (defun start-org-drill ()
              (interactive)
              (org-id-goto "org-heading-for-org-drill") ; id PROPERTY of my Org-Drill Heading
              (org-narrow-to-subtree)
              (org-drill)
              (save-buffer))))

;; My Org Blog Setup
(use-package blog
  :load-path "lisp/blog.el")

;; Org-Music Mode
(use-package org-music
  :load-path "lisp/org-music.el"
  :init (progn
          (setq
           org-music-file "~/Notes/Music.org"
           org-music-media-directory "~/Music/Sync/Org/"
           org-music-next-cloud-script "~/Code/bin/nextcloud.py"
           org-music-operating-system "linux"
           org-music-playlist-file "orgmusic-linux.m3u"
           org-music-cache-size 100)
          (add-hook
           'org-mode-hook
           (lambda()
             (if (equal buffer-file-name (expand-file-name org-music-file))
                 (org-music-mode))))))

;; Org-Media-Annotation Mode
(use-package org-media-annotation
  :load-path "lisp/org-media-annotation.el")

;; Org QL
(use-package org-ql
  :ensure t)

;; Set SBCL as default lisp interpreter
(if (executable-find "sbcl") (setq inferior-lisp-program (executable-find "sbcl")))

;; Try evil for modal navigation, editing
(use-package evil :ensure t)
;(use-package evil-org :ensure t)

;; Csharp Mode
(use-package csharp-mode
  :ensure t)

;; Elpy for Python
(use-package elpy
  :ensure t
  :commands elpy-enable
  :init (with-eval-after-load 'python (elpy-enable))
  :config (progn
            (add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))
            (setq
             elpy-test-nose-runner-command '("nosetests" "-s" "-v")
             elpy-test-runner 'elpy-test-nose-runner)))

;; Make ipython 5.x (color)compatible with Emacs eshell
(if (executable-find "ipython") (setq python-shell-interpreter "ipython" python-shell-interpreter-args "--simple-prompt -i"))

;; Clojure
(use-package clojure-mode
  :ensure t
  :hook (clojure-mode . eldoc-mode)
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode)))

(use-package cider
  :ensure t
  :defer t
  :hook (cider-mode . eldoc-mode)
  :init (add-hook 'cider-mode-hook #'clj-refactor-mode)
  :diminish subword-mode
  :config
  (setq nrepl-log-messages t
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t
        ; cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        nrepl-hide-special-buffers t
        cider-overlays-use-font-lock t)
  (cider-repl-toggle-pretty-printing))

(use-package clj-refactor
  :defer t
  :ensure t
  :diminish clj-refactor-mode
  :config (cljr-add-keybindings-with-prefix "C-c C-m"))

;; Rust Mode
(use-package rust-mode :ensure t)
(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

;; Install Haskell-Mode
(use-package haskell-mode :ensure t)

;; Dante for Haskell
(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode))

;; Tidal for Live Coding Music in Haskell
(use-package tidal
  :ensure t)

;; Web-Mode for HTML
(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
         ("\\.tsx\\'" . web-mode))
  :config (progn
            (add-hook 'web-mode-hook
                      (lambda ()
                        (when (string-equal "tsx" (file-name-extension buffer-file-name))
                          (setup-tide-mode))))
            (add-hook 'web-mode-hook
                      (lambda ()
                        (setq web-mode-enable-css-colorization t
                              web-mode-markup-indent-offset 4
                              web-mode-css-indent-offset 4
                              web-mode-code-indent-offset 4
                              web-mode-style-padding 4
                              web-mode-script-padding 4
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
            (setq-default js2-basic-offset 4
                          js2-indent-switch-body t
                          js2-auto-indent-p t
                          flycheck-disabled-checkers '(javascript-jshint)
                          flycheck-checkers '(javascript-eslint)
                          flycheck-eslintrc "~/.eslintrc"))
  :hook (tern-mode .js2-mode))

;; Tern for Javascript
(use-package tern :ensure t :defer t)
(use-package tern-auto-complete
  :ensure t
  :commands tern-ac-setup
  :init (with-eval-after-load 'tern (tern-ac-setup)))

;; Typescript
(defun setup-tide-mode ()
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq typescript-indent-level
        (or (plist-get (tide-tsfmt-options) ':indentSize) 4))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package tide
  :ensure t
  :hook ((typescript-mode . setup-tide-mode)
         (before-save . tide-format-before-save))
  :config (progn
            (flycheck-add-mode 'typescript-tslint 'web-mode)))

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
            (setq rmh-elfeed-org-files (list (expand-file-name "~/Notes/Feed.org")))))

;; elfeed helper functions
(defun elfeed-load ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load) ;; this was commented earlier due to strange behavior
  (elfeed)
  (elfeed-search-update :force))
(defun deb/elfeed-frontpage ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-frontpage"))
(defun deb/elfeed-save-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))
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
(defun deb/elfeed-listen ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-listen"))
(defun deb/elfeed-watch ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-watch"))

(defun elfeed-junk-filter (entry)
  (when (or (string-match "thestranger" (elfeed-entry-link entry))
            (string-match "ycombinator" (elfeed-entry-link entry)))
    (let ((title (elfeed-entry-title entry))
          (content (elfeed-deref (elfeed-entry-content entry)))
          (trump "\\<trump\\>")
          (case-fold-search t))
      (unless (or (string-match trump title)
                  (string-match trump content))
        (elfeed-tag entry 'junk)))))

(add-hook 'elfeed-new-entry-hook #'elfeed-junk-filter)

(use-package elfeed
  :ensure t
  :defer t
  :init (setq elfeed-db-directory "~/Notes/.elfeed")
  :bind (:map elfeed-search-mode-map
              ("h" . deb/elfeed-hack)
              ("u" . deb/elfeed-understand)
              ("e" . deb/elfeed-experience)
              ("f" . deb/elfeed-frontpage)
              ("l" . deb/elfeed-listen)
              ("w" . deb/elfeed-watch)
              ("A" . deb/elfeed-all)
              ("q" . deb/elfeed-save-bury)))

(use-package elfeed-web :ensure t :defer t)

;; EMMS - Emacs Media Player
(use-package emms
  :ensure t
  :defer t
  :bind
  (("C-c e SPC" . emms-pause)
   ("C-c e j" . emms-previous)
   ("C-c e k" . emms-next)
   ("C-c e h" . emms-seek-backward)
   ("C-c e l" . emms-seek-forward)
   ("C-c e n" . emms-volume-lower)
   ("C-c e i" . emms-volume-raise))
  :config
  (progn
    (emms-all)
    ;(emms-history-load)
    (setq emms-player-list (list emms-player-mpv)
          emms-source-file-default-directory (expand-file-name "~/Music")
          emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find
          emms-browser-covers 'emms-browser-cache-thumbnail
          emms-playlist-buffer-name "*Music Playlist*"
          emms-playlist-mode-open-playlists t
          emms-player-mpv-parameters (append
                                      emms-player-mpv-parameters
                                      '("--ytdl-format=bestaudio")))

    (add-to-list 'emms-info-functions 'emms-info-cueinfo)

    (if (executable-find "emms-print-metadata")
        (progn
          (require 'emms-info-libtag)
          (add-to-list 'emms-info-functions 'emms-info-libtag)
                (delete 'emms-info-ogginfo emms-info-functions)
                (delete 'emms-info-mp3info emms-info-functions))
      (add-to-list 'emms-info-functions 'emms-info-ogginfo)
      (add-to-list 'emms-info-functions 'emms-info-mp3info))

    (defun ambrevar/emms-play-on-add (old-pos)
      "Play tracks when calling `emms-browser-add-tracks' if nothing
       is currently playing."
      (interactive)
      (when (or (not emms-player-playing-p)
                emms-player-paused-p
                emms-player-stopped-p)
        (with-current-emms-playlist
          (goto-char old-pos)
          ;; if we're sitting on a group name, move forward
          (unless (emms-playlist-track-at (point))
            (emms-playlist-next))
          (emms-playlist-select (point)))
        (emms-stop)
        (emms-start)))
    (add-hook 'emms-browser-tracks-added-hook 'ambrevar/emms-play-on-add)))

(use-package diminish
  :ensure t
  :diminish auto-revert-mode
  :diminish abbrev-mode
  :diminish undo-tree-mode
  :diminish which-key-mode
  :diminish subword-mode
  :diminish eldoc-mode
  :diminish ivy-mode
  :diminish whitespace-cleanup-mode)

(eval-after-load 'flycheck
  '(diminish 'flycheck-mode))

(eval-after-load 'auto-revert-mode
  '(diminish 'auto-revert-mode))

(eval-after-load 'whitespace-cleanup-mode
  '(diminish 'whitespace-cleanup-mode))

;; ---------------
;; Custom Macros
;; ---------------
;; Note: Replace current book name used in Macro to the name of current book for which highlights are to be converted from Bookcision Plain-Text import to Org-Mode
;;       E.g Sapiens = ?S ?a ?p ?i ?e ?n ?s with New Book = ?N ?e ?w ?  ?B ?o ?o ?k
(fset 'convert-kindle-highlight-from-bookcision-plain-text-to-org-mode
      (kmacro-lambda-form [?\C-s ?L ?O ?C ?A ?T ?I ?O ?N ?: return ?\C-a ?\C-k C-up ?\C-p ?* ?* ?  ?\C-y ?\C-a ?\M-c ?\C-d ?\C-a ?\M-x ?o ?r ?g ?i ?d ?g ?e ?t ?c ?r ?e ?a ?t ?e return ?\C-n ?\C-e tab return tab ?: ?\C-y return tab ?: ?B ?O ?O ?K ?: ?  ?  ?  ?  ?  ?S ?a ?p ?i ?e ?n ?s ?: ?  ?A ?\S-  ?B ?r ?i ?e ?f ?  ?H ?i ?s ?t ?o ?r ?y ?  ?o ?f ?  ?H ?u ?m ?a ?n ?k ?i ?n ?d ?  ?\( ?Y ?u ?v ?a ?l ?  ?N ?o ?a ?h ?  ?H ?a ?r ?a ?r ?i ?\) C-down tab ?\C-  ?\C-e ?\M-q ?\C-  right ?\C-  ?\C- ] 0 "%d"))

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
          (setq solarized-use-variable-pitch nil)
          (setq cursor-color "aaaaaa"))
  :config (progn
            (load "solarized-theme-autoloads" nil t)
            (load-theme 'solarized-light t)))
