; Pre-init setup. Useful for ESUP profiling
(setq use-short-answers t)  ; Answer with y or n instead of the default yes or no
(setq vc-follow-symlinks t) ; Do Not Ask to Follow VC Symlinks

;; Install Straight.el as Package Manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Setup use-package
(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

;; ---------------
;; Org Mode
;; ---------------
(use-package all-the-icons) ;; icon set

;; Setup ESUP to Profile Emacs
(use-package esup
  :init (setq esup-depth 0)) ; fix to make ESUP work with straight.el

;; Mu4e For Mail on Emacs
(use-package mu4e-org
  :straight (mu4e-org
             :type git
             :host github
             :repo "djcb/mu"
             :files (:defaults "mu4e/mu4e-org.el")))

;; Org-Mode
(use-package org
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

             ;; Allow user specified scaling of org-inline images
             org-image-actual-width nil

             ;; Allow acting on multiple entries selected in buffer or agenda
             ;; e.g To bulk update todo state, scheduled time etc
             org-loop-over-headlines-in-active-region t
             org-agenda-loop-over-headlines-in-active-region t

             ;; Indent entry body to level of heading
             org-adapt-indentation t

             ;; Allow using subscript and superscript with _{}, ^{} respectively
             org-use-sub-superscripts '{}
             org-pretty-entities-include-sub-superscripts t

             ;; Display text as UTF-8
             org-pretty-entities t

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

             ;; Attachments
             org-attach-id-dir "~/Notes/data/"

             ;; Org-Babel
             ;; set cider as org babel clojure backend
             org-babel-clojure-backend 'cider
             ;; execute without confirmation
             org-confirm-babel-evaluate nil
             ;; Syntax highlighting of code blocks
             org-src-fontify-natively t
             ;; Auto-indent of code blocks
             org-src-tab-acts-natively t
             ;; path to ditaa jar
             org-ditaa-jar-path "~/Code/bin/ditaa.jar"

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

             ;; Define Stuck Projects
             org-stuck-projects
             '("+LEVEL=2/-CANCELLED-DONE-FAILED" ("TODO" "ACTIVE" "WAITING") nil "")

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
             org-tag-alist '((:startgroup) ("HACK" . ?h) ("UNDERSTAND" . ?u) ("EXPERIENCE" . ?e) (:endgroup)
                             (:startgroup) ("TRY" . ?t) ("MAINTAIN" . ?n) ("FIX" . ?x) ("UPGRADE" . ?r) (:endgroup)
                             (:startgroup) ("PERSONAL" . ?p) ("ENVIRONMENT" . ?v) ("SOCIAL" . ?s) ("PROJECT" . ?j) (:endgroup)
                             (:startgroup) ("READ" . ?R) ("WATCH" . ?W) ("LISTEN" . ?L)  ("WRITE" . ?w) (:endgroup)
                             ("TOOLS" . ?g) ("CALL" . ?a) ("BUY" . ?y) ("IDLE" . ?d) ("HEALTH" . ?l) ("FINANCE" . ?f))

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
            ;; (require 'cider)
            ;; (load "ob-sudo.el")

            ;; Redisplay images after org-babel execute
            (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

            ;; Setup Babel languages. Can now do Literate Programming
            (setq org-babel-python-command "python3")
            (org-babel-do-load-languages 'org-babel-load-languages
                                         '((python . t)
                                           (shell . t)
                                           (emacs-lisp . t)
                                           (ledger . t)
                                           (ditaa . t)
                                           (plantuml . t)
                                           (clojure . t)
                                           (js . t)
                                           (C . t)))
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

            (defun create-open-deep-link (start end)
              (interactive "r")
              (let* ((highlighted-text (buffer-substring-no-properties start end))
                     (resource-link (org-entry-get start "RESOURCE"))
                     (deep-link (format "%s/#:~:text=%s" resource-link highlighted-text)))
                (org-link-open-from-string (url-encode-url deep-link))))

            ;;store org-mode links to messages
            (require 'mu4e-org)
            ;;store link to query if in header view, not the message cursor is currently on
            (setq mu4e-org-link-query-in-headers-mode t)

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

(use-package org-contrib :after org)
(use-package org-depend :after org-contrib)

;; Orgit: Support for Org links to Magit buffers
(use-package orgit :after (org magit))

;; Annotation for ebooks, pdf's etc in org files
(use-package org-noter
  :after org
  :config (progn
            (setq
             org-noter-auto-save-last-location t
             org-noter-notes-search-path '("~/Notes"))))

(use-package org-randomnote
  :after org
  :bind ("C-c r" . org-randomnote)
  :config (setq org-randomnote-candidates '("~/Notes/Schedule.org" "~/Notes/Incoming.org" "~/Notes/Archive.org")))

(use-package clip2org
  :straight (clip2org :type git
                      :host github
                      :repo "thamer/clip2org"
                      :fork (:type git
                             :host github
                             :repo "debanjum/clip2org"))
  :config (setq clip2org-clippings-file
                (expand-file-name "~/Documents/eBooks/My Clippings.txt")))

;; Drag and drop images/files to attach to org task
(use-package org-download
  :after org
  :config
  ;; add support to dired
  (add-hook 'dired-mode-hook 'org-download-enable)
  (setq org-download-screenshot-method "/usr/sbin/screencapture -s %s")
  (setq org-download-method 'attach))

(use-package ob-tmux
  :after org
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
  :after org
  :bind ("C-x p i" . 'org-cliplink))

(use-package org-mime :after org)

(use-package org-drill
  :after org
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

;; Org QL
(use-package org-ql :after org)
(use-package helm-org-ql :after org)

(use-package plantuml-mode
  :after org
  :config
  (progn
    (setq
     org-plantuml-jar-path
     (expand-file-name "~/Builds/PlantUML/plantuml.1.2020.9.jar"))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))))

;; Support Transclusion links in Org-Mode
;; *disabled*: Org-Transclusion deleting large amounts of text content on file save for some reason
;;(use-package org-transclusion
;;  :after org
;;  :bind (:map org-mode-map
;;              ("C-c t a" . 'org-transclusion-add)
;;              ("C-c t l" . 'org-transclusion-make-from-link)))

;; My Org Blog Setup
;;(use-package blog
;;  :after org
;;  :straight (blog :local-repo "~/Code/Lisp/Blog" :type git :require (ox-rss ox-html)))


;; ---------------
;; Basic Config
;; ---------------
;; Disable menu, toolbar, scrollbar
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
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
(desktop-save-mode)

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

;; Set default tab width
(setq-default tab-width 4)
;; C spacing = 4 instead of default 2
(setq-default c-basic-offset 4)

;; enable some advanced commands
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; backup, autosaves in single, flat directory
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
; (setq auto-save-file-name-transforms '(("" . "~/.emacs.d/backup")))

;; forward, previous paragraph
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

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

(defun toggle-fullscreen ()
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value) 'fullboth)))))

;; Puts custom-set-variables into a separate temporary file
(setq custom-file (make-temp-file "emacs-custom"))

;; Spell Check
(when (executable-find "aspell")
  (setq-default ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "--camel-case")))

;; Cache passphrase for symmetric encrypted files
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

(require 'uniquify)
(setq
 ;; Uniquify buffer names foo.c:src, foo.c:extra
 uniquify-buffer-name-style 'reverse uniquify-separator ":"

 ;; rename after killing uniquified
 uniquify-after-kill-buffer-p t

 ;; ignore special buffers
 uniquify-ignore-buffers-re "^\\*")

;; no tabs by default. modes that really need tabs should enable
;; indent-tabs-mode explicitly. e.g makefile-mode already does that
(setq-default indent-tabs-mode nil)

;; Add user elisp load path
(add-to-list 'load-path "~/.emacs.d/personal/")

;; Setup Mail: mu4e, smtpmail
;(load "setup-mail.el")

;; Load custom macros
(load "custom-macros.el")

;; Load private configuration
(load "private.el")

;; ---------------
;; Tools
;; ---------------
;; Khoj for Search, Chat
(use-package khoj
  :after org
  :straight (khoj
             :type git
             :host github
             :repo "khoj-ai/khoj"
             :files ("src/interface/emacs/khoj.el"))
  :bind ("C-c s" . 'khoj)
  :config (setq
           khoj-server-url "http://localhost:42110"
           khoj-chat-offline t
           khoj-openai-api-key openai-api-token
           khoj-chat-model "gpt-4"
           khoj-org-files '("~/Notes/Incoming.org" "~/Notes/Schedule.org" "~/Notes/Archive.org" "~/Notes/Kindle.org")))

(use-package dired
  :straight (:type built-in)
  :bind (:map dired-mode-map
              ("C-c C-e" . wdired-change-to-wdired-mode))
  :init
  (setq dired-dwim-target t
        dired-recursive-copies 'top
        dired-recursive-deletes 'top
        dired-use-ls-dired nil
        dired-listing-switches "-alh")
  :hook (dired-mode . dired-hide-details-mode))

;; Useful commandlets
(use-package crux
  :straight (crux :type git
                  :host github
                  :repo "bbatsov/crux"
                  :fork (:type git
                         :host github
                         :repo "debanjum/crux"))
  :bind (("C-k" . crux-smart-kill-line)          ;; kill line from point or whole line
         ("C-a" . crux-move-beginning-of-line))) ;; Jump to first non-whitespace character on line or beginning of line

;; Allow defining keybinding chords via use-package
(use-package use-package-chords
  :config (key-chord-mode 1))

(use-package which-key
  :diminish which-key-mode
  :init (which-key-mode))

(use-package avy
  :init
  (setq avy-keys-alist
        `((avy-goto-char-timer . (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
          (avy-goto-line . (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i)))
        avy-style 'pre)
  :bind* (("M-m f" . avy-goto-char-timer)
          ("M-m F" . avy-goto-line)
          (:map isearch-mode-map ("C-'" . avy-isearch))))

(use-package undo-tree
  :after (use-package-chords)
  :chords (("uu" . undo-tree-visualize))
  :diminish undo-tree-mode
  :config (progn (global-undo-tree-mode 1)
                 (setq
                  undo-tree-visualizer-timestamps t
                  ;; Prevent undo tree files from polluting directories with undo-tree history
                  undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree"))
                  undo-tree-visualizer-diff t)

                 ;; Suppress the message saying that the undo history file was
                 ;; saved (because this happens every single time you save a file).
                 ;; borrowed from raxod502's radian emacs config
                 (defun radian--undo-tree-suppress-undo-history-saved-message
                     (undo-tree-save-history &rest args)
                   (let ((inhibit-message t))
                     (apply undo-tree-save-history args)))
                 (advice-add #'undo-tree-save-history :around #'radian--undo-tree-suppress-undo-history-saved-message)

                 ;; Suppress the message saying that the undo history could not be
                 ;; loaded because the file changed outside of Emacs.
                 (defun radian--undo-tree-suppress-buffer-modified-message
                     (undo-tree-load-history &rest args)
                   (let ((inhibit-message t))
                     (apply undo-tree-load-history args)))
                 (advice-add #'undo-tree-load-history :around #'radian--undo-tree-suppress-buffer-modified-message)
                 ))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy))

;; Expand-Region for intelligent highlight expansion
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Faster window switching
(use-package ace-window
  :bind (("C-x o" . ace-window))
  :config (progn
            (setq aw-keys '(?a ?s ?k ?l ?d ?j ?h ?f ?g))))

;; Ivy for completion function everywhere where ido isn't used (for now)
(use-package ivy
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-c C-j" . ivy-immediate-done)) ; default keybinding C-M-j conflicts with an MacOS keybinding
  :config (progn
            (ivy-mode)
            (setq
             ivy-wrap t
             ivy-use-virtual-buffers t
             enable-recursive-minibuffers t
             ivy-count-format ""
             ivy-initial-inputs-alist nil
             ivy-sort-matches-functions-alist '((t . nil)) ;; To sort most recent first
             ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                     (t      . ivy--regex-fuzzy)))))

(use-package counsel
  :after (ivy use-package-chords)
  :diminish
  :chords (("yy" . counsel-yank-pop))
  :config (progn
            (counsel-mode)))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper-backward)))

;; Amx for M-x persistent MRU for auto-completion. Replaces Smex as that's deprecated.
;; Amx can rely on Ivy via counsel-M-x or ido completion backend for fuzzy candidate matching etc.
(use-package amx
  :config (amx-mode))

;; Recentf to suggest recently opened files on C-x C-r
(use-package recentf
  :init (recentf-mode t)
  :bind ("C-x C-r" . counsel-recentf)  ;; replace `find-file-read-only` with more a useful command
  :config (setq recentf-max-saved-items 50))

(use-package outline
  :straight (:type built-in)
  :bind (:map outline-minor-mode-map
              ("C-u C-c C-o" . outline-cycle-buffer)
              ("C-c C-o" . outline-cycle)
              ("C-c C-n" . outline-next-visible-heading)
              ("C-c C-p" . outline-previous-visible-heading)
              ("C-c C-f" . outline-forward-same-level)
              ("C-c C-b" . outline-backward-same-level)
              ("C-c C-u" . outline-up-heading)))

;; To keep GNU ELPA Keyring up-to-date
(use-package gnu-elpa-keyring-update)

;; Gnuplot for Plotting in org
(use-package gnuplot :defer t)

;; ag - the silver searcher
(use-package ag
  :commands (ag ag-regexp ag-project)
  :config (setq ag-highlight-search t))

;; Magit for Git
(use-package magit
  :bind ("C-x g" . magit-status)
  :config (setq magit-diff-refine-hunk 'all))

;; Magit Find File
(use-package magit-find-file
  :after magit
  :bind ("C-c p" . magit-find-file-completing-read)
  :config (setq magit-completing-read-function 'ivy-completing-read))

(use-package forge
  :after magit
  :config (setq forge-owned-accounts '(("debanjum"))))

;; Company mode for Completion
(use-package company
  :defer t
  :diminish company-mode
  :config (setq
           ;; trigger completion immediately.
           company-idle-delay 0.2

           ;; number the candidates (use M-1, M-2 etc to select completions).
           company-show-numbers t))

;; Beancount Company backend
(use-package company-ledger
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-ledger)))

;; Beancount Minor Mode
(use-package beancount
  :straight (beancount :type git :host github :repo "beancount/beancount-mode")
  :after company
  :hook ((beancount-mode . company-mode)
         (beancount-mode . outline-minor-mode))
  :mode ("\\.bean\\'" . beancount-mode)
  :config (setq
           beancount-number-alignment-column 72))

;; Nov.el Epub Reader Mode
(use-package nov
  :ensure xml+
  :mode ("\\.epub\\'" . nov-mode))

(use-package package-lint :defer t) ; useful when developing elisp packages
(use-package flycheck
  :defer t
  :config (setq flycheck-emacs-lisp-load-path 'inherit))

;; whitespace-cleanup-mode. remove whitespaces on buffer save
(use-package whitespace-cleanup-mode
  :hook ((python-mode . whitespace-cleanup-mode)
         (org-mode . whitespace-cleanup-mode))
  :init (add-hook
           'write-file-functions
           (lambda () (untabify (point-min) (point-max)) nil)))

;; Code Generation, Completion using OpenAI Codex
(use-package codex-completion
  :straight (codex-completion :type git :host github :repo "debanjum/codex-completion")
  :bind ("C-c ." . codex-completion)
  :config (setq codex-completion-openai-api-token openai-api-token))

;; Used to zoom in/out across all buffers
(use-package default-text-scale)

;; Allows folding active region
(use-package fold-this)

;; Hydras to group similar functions into menus
(use-package hydra
  :config
  (progn
    (defhydra hydra-zoom ()
      "zoom"
      ("-" text-scale-decrease "out")         ; shadows default behavior
      ("+" text-scale-increase "in")          ; shadows default behavior
      ("0" (progn (text-scale-increase 0) (default-text-scale-reset)) "reset")   ; shadows default behavior 
      ("(" (default-text-scale-decrease) "global zoom-out")
      (")" (default-text-scale-increase) "global zoom-in")
      ("f" (toggle-fullscreen) "fullscreen"))
    (add-hook 'text-scale-mode-hook 'hydra-zoom/body)

    (defhydra hydra-window
      (global-map "C-c w"
       :color red
       :hint nil
       :pre (progn (winner-mode 1) (require 'hydra-examples))
       :post (winner-mode -1))
      "
 Split: _v_ert _x_:horz
Delete: _o_nly  _da_ce  _dw_indow  _db_uffer  _df_rame
  Move: _s_wap
Frames: _f_rame new  _df_ delete
  Misc: _a_ce  _u_ndo  _r_edo
      "
      ("h" windmove-left)
      ("j" windmove-down)
      ("k" windmove-up)
      ("l" windmove-right)
      ("H" hydra-move-splitter-left)
      ("J" hydra-move-splitter-down)
      ("K" hydra-move-splitter-up)
      ("L" hydra-move-splitter-right)
      ("|" (lambda ()
             (interactive)
             (split-window-right)
             (windmove-right)))
      ("_" (lambda ()
             (interactive)
             (split-window-below)
             (windmove-down)))
      ("v" split-window-right)
      ("x" split-window-below)
      ("u" winner-undo)
      ("r" winner-redo) ;;Fixme, not working?
      ("o" delete-other-windows :exit t)
      ("a" ace-window :exit t)
      ("f" new-frame :exit t)
      ("s" ace-swap-window)
      ("da" ace-delete-window)
      ("dw" delete-window)
      ("db" kill-this-buffer)
      ("df" delete-frame :exit t)
      ("q" nil)
      ("b" ivy-switch-buffer))

    (defhydra hydra-ediff
      (global-map "C-c d"
       :color blue
       :hint nil)
      "
^Buffers           Files           VC                     Ediff regions
----------------------------------------------------------------------
_b_uffers           _f_iles (_=_)       _r_evisions              _l_inewise
_B_uffers (3-way)   _F_iles (3-way)                          _w_ordwise
                  _c_urrent file
"
      ("b" ediff-buffers)
      ("B" ediff-buffers3)
      ("=" ediff-files)
      ("f" ediff-files)
      ("F" ediff-files3)
      ("c" ediff-current-file)
      ("r" ediff-revision)
      ("l" ediff-regions-linewise)
      ("w" ediff-regions-wordwise))

    (defhydra hydra-hideshow
      (global-map "C-c f"
       :color pink
       :hint nil
       :idle 1.0
       :pre (hs-minor-mode 1))
      "
Hide^^            ^Show^            ^Toggle^    ^Navigation^
----------------------------------------------------------------
_h_ hide all      _s_ show all      _t_oggle    _n_ext line
_d_ hide block    _a_ show block              _p_revious line
_l_ hide level
_r_ hide region
---------
_q_ quit
"
      ("s" (progn (hs-show-all) (fold-this-unfold-all)) "show all")
      ("h" hs-hide-all "hide all")
      ("a" hs-show-block "show block")
      ("d" hs-hide-block "hide block")
      ("t" hs-toggle-hiding "toggle hiding")
      ("l" hs-hide-level "hide level")
      ("r" fold-this "fold active region")
      ("n" forward-line "next line")
      ("p" (forward-line -1) "previous line")
      ("q" nil "quit" :color blue))
    )
  )

(use-package mu4e-alert
  :ensure t
  :after mu4e
  :straight (mu4e-alert :type git :host github :repo "xzz53/mu4e-alert")
  :init
  (setq mu4e-alert-interesting-mail-query
    (concat
     "flag:unread maildir:/riseup/INBOX "
     "OR "
     "flag:unread maildir:/gmail/INBOX"
     ))
  (mu4e-alert-enable-mode-line-display)
  (defun refresh-mu4e-alert-mode-line ()
    (interactive)
    (mu4e--server-kill)
    (mu4e-alert-enable-mode-line-display)
    )
  (run-with-timer 0 60 'refresh-mu4e-alert-mode-line))

;; Display command or keybinding to help in demos
(use-package keycast)

;; ---------------
;; Language Packages
;; ---------------
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :hook ((prog-mode . copilot-mode)
         (mu4e-compose-mode . copilot-mode))
  :bind (:map copilot-completion-map
              ("C-c <right>" . copilot-next-completion)
              ("C-c <left>" . copilot-previous-completion))
  :config (progn
            (setq copilot-node-executable "~/.nvm/versions/node/v16.18.1/bin/node")
            (setq copilot-idle-delay 0.8)
            (defun my/copilot-tab ()
              (interactive)
              (or (copilot-accept-completion-by-paragraph)
                  (indent-for-tab-command)))

            (with-eval-after-load 'copilot
              (define-key copilot-mode-map (kbd "<tab>") #'my/copilot-tab))))

(use-package ruby-mode
  :mode "\\.rb\\'")

;; Set SBCL as default lisp interpreter
(if (executable-find "sbcl") (setq inferior-lisp-program (executable-find "sbcl")))

;; Use rainbow delimiters for lisps
(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (clojure-mode . rainbow-delimiters-mode)))

;; Try evil for modal navigation, editing
(use-package evil :defer t)
;;(use-package evil-org :ensure t)

;; Csharp Mode
(use-package csharp-mode :defer t)

;; Elpy for Python
;;(use-package elpy
;;  :commands elpy-enable
;;  :init (with-eval-after-load 'python (elpy-enable))
;;  :config (progn
;;            (add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))
;;            (setq
;;             elpy-test-nose-runner-command '("nosetests" "-s" "-v")
;;             elpy-test-runner 'elpy-test-nose-runner)))

;; Make ipython 5.x (color)compatible with Emacs eshell
(if (executable-find "ipython") (setq python-shell-interpreter "ipython" python-shell-interpreter-args "--simple-prompt -i"))

;; Emacs IPython Notebook (EIN) OR Jupyter Notebooks
(use-package ein :after org)

;; Clojure
(use-package clojure-mode
  :hook (clojure-mode . eldoc-mode)
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode)))

(use-package cider
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
  :diminish clj-refactor-mode
  :config (cljr-add-keybindings-with-prefix "C-c C-m"))

;; Rust Mode
(use-package rust-mode :defer t)
(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

;; Install Haskell-Mode
(use-package haskell-mode :defer t)

;; Dante for Haskell
(use-package dante
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode))

;; Tidal for Live Coding Music in Haskell
(use-package tidal :defer t)

;; Web-Mode for HTML
(use-package web-mode
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
(use-package tern :defer t)
(use-package tern-auto-complete
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
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


;; ---------------
;; Feed Reader
;; ---------------
(use-package elfeed
  :defer t
  :init (setq elfeed-db-directory "~/Notes/.elfeed")
  :straight (elfeed :type git
                    :host github
                    :repo "skeeto/elfeed"
                    :fork (:type git
                           :host github
                           :repo "debanjum/elfeed"
                           :branch "master"))
  :bind (:map elfeed-search-mode-map
              ("h" . (lambda () (interactive)
                       (elfeed-search-set-filter "@1-months-ago +unread +hack")))
              ("u" . (lambda () (interactive)
                       (elfeed-search-set-filter "@1-months-ago +unread +understand")))
              ("e" . (lambda () (interactive)
                       (elfeed-search-set-filter "@1-months-ago +unread +experience")))
              ("f" . (lambda () (interactive)
                       (elfeed-search-set-filter "@1-months-ago +unread +fp")))
              ("l" . (lambda () (interactive)
                       (elfeed-search-set-filter "@1-months-ago +unread +listen")))
              ("w" . (lambda () (interactive)
                       (elfeed-search-set-filter "@1-months-ago +unread +watch")))
              ("A" . (lambda () (interactive)
                       (elfeed-search-set-filter "@1-months-ago +unread")))
              ("q" . deb/elfeed-save-bury)))

;; Organise feeds in an Org file
(use-package elfeed-org
  :after (org elfeed)
  :config (progn
            (elfeed-org)
            (setq rmh-elfeed-org-files (list (expand-file-name "~/Notes/Feed.org")))))

(use-package elfeed-web :after elfeed :defer t)

;; Improve Elfeed Youtube RSS Feeds Integration
(use-package elfeed-tube
  :after elfeed
  :defer t
  :config (progn
            (setq elfeed-tube-auto-save-p nil)
            (setq elfeed-tube-auto-fetch-p t)
            (elfeed-tube-setup))
  :bind (:map elfeed-show-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)
         :map elfeed-search-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)))
(use-package mpv)
(use-package elfeed-tube-mpv
  :after elfeed
  :defer t
  :bind (:map elfeed-show-mode-map
              ("C-c C-f" . elfeed-tube-mpv-follow-mode)
              ("C-c C-w" . elfeed-tube-mpv-where)))

;; Elfeed helper functions
(defun elfeed-load ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load) ;; this was commented earlier due to strange behavior
  (elfeed)
  (elfeed-search-update :force))
(defun deb/elfeed-save-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))
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

;; ---------------
;; Music Player
;; ---------------

;; EMMS - Emacs Media Player
(use-package emms
  :straight (emms
             :type git
             :repo "https://git.savannah.gnu.org/git/emms.git"
             :fork (:type git
                    :host github
                    :repo "debanjum/emms"))
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

;; Org-Music Mode
(use-package org-music
  :straight (org-music :type git :host github :repo "debanjum/org-music")
  :after (org emms)
  :init (progn
          (setq
           org-music-file "~/Notes/Music.org"
           org-music-media-directory "~/Music/Sync/Org/"
           org-music-next-cloud-script "~/Code/bin/nextcloud.py"
           org-music-operating-system "mac"
           org-music-playlist-file "orgmusic-mac.m3u"
           org-music-cache-size 100)
          (add-hook
           'org-mode-hook
           (lambda()
             (if (equal buffer-file-name (expand-file-name org-music-file))
                 (org-music-mode))))))

;; Org-Media-Annotation Mode
(use-package org-media-annotation
  :after (emms org)
  :straight
  (org-media-annotation
   :type git
   :host github
   :protocol ssh
   :repo "debanjum/org-media-annotation"))

;; -------------
;; Org Mode Hack
;; -------------
;; Issue: Marking recurring task as done from agenda would update other recurring items too
;; Mitigation: Configuring these org settings towards the end of my init.el fixes the issue 🤷🏾
(setq
 ;; Allow acting on multiple entries selected in buffer or agenda
 ;; e.g To bulk update todo state, scheduled time etc
 org-loop-over-headlines-in-active-region t
 org-agenda-loop-over-headlines-in-active-region t

 ;; Indent entry body to level of heading
 org-adapt-indentation t)

;; ---------------
;; Diminish Modes
;; ---------------
(use-package diminish
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
;; Theme
;; ---------------
(defun toggle-dark-mode ()
  "Toggle dark and light mode theme"
  (interactive)
  (cond
   ((eq 'my-solarized-light (car custom-enabled-themes))
       (load-theme 'my-solarized-dark t))
   (t (load-theme 'my-solarized-light t))))

;; Solarized Emacs Theme @ https://github.com/bbatsov/solarized-emacs
;; If theme not loaded => init.el (partial) failed
(use-package solarized-theme
  :init (progn
          (setq
           ;; set theme directory
           custom-theme-directory (expand-file-name "themes" user-emacs-directory)
           ;; Don't scale outline headings based on their depth
           solarized-scale-org-headlines nil
           solarized-scale-outline-headlines nil
           ;; Don't change the font for some headings and titles
           solarized-use-variable-pitch nil))
  :config (progn
            (load "solarized-theme-autoloads" nil t)
            (load-theme 'my-solarized-light t)))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))
