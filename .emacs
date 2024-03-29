;; ----------------------------------
;; PACKAGE LIST
;; ----------------------------------

(setq package-list	
  '(python-mode yaml-mode markdown-mode dockerfile-mode docker-compose-mode robot-mode go-mode htmlize))

;; ----------------------------------
;; PACKAGE INSTALLATION
;; ----------------------------------

(require 'package)
(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 0)))

(add-to-list 'load-path "~/.emacs.d/lisp/")
;; Ensures we can download packages on debian
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;; Initialize package list
(package-initialize)
; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))
; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; ----------------------------------
;; GENERAL
;; ----------------------------------

;; theme
(load-theme 'whiteboard)
;; follow symlinks automatically when opening files
(setq vc-follow-symlinks t)
;; store all backup and autosave files in a separate dir
(defvar --backup-directory (concat user-emacs-directory "auto-backups"))
(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq backup-by-copying t)
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )
;; automatically reload file changes
(global-auto-revert-mode 1)

;; ----------------------------------
;; TEXT EDITING
;; ----------------------------------

;; default tab width
(setq-default tab-width 4)
;; use spaces instead of tabs
(setq-default indent-tabs-mode 0)
;; make text mode the default major mode
(setq default-major-mode 'text-mode)
;; turn on auto-fill
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; ----------------------------------
;; FRAME LAYOUT
;; ----------------------------------

;; show line numbers on each line
(global-display-line-numbers-mode)
;; show column number in status bar
(setq column-number-mode t)

;; ----------------------------------
;; WEB BROWSING
;; ----------------------------------

;; always open web links in emacs browser
(setq browse-url-browser-function 'eww-browse-url)

;; ----------------------------------
;; ORG MODE
;; ----------------------------------

;; add all default org files to agenda
(setq org-agenda-files '("~/org"))
;; turn on auto fill
(add-hook 'org-mode-hook 'turn-on-auto-fill)
;; add live and wait to keywords
(setq org-todo-keywords '((sequence "TODO(t)" "LIVE(l)" "WAIT(w)" "|" "DONE(d)")))
(setq org-todo-keyword-faces '(("LIVE" . "blue") ("WAIT" . "magenta")))
;; configure agenda view
(setq org-agenda-span 1
      org-agenda-start-on-weekday nil
      org-agenda-start-day "-0d")
(setq org-deadline-warning-days 0)
(setq org-log-into-drawer t)
(setq org-agenda-todo-ignore-scheduled 'all)
(setq org-agenda-todo-ignore-deadline 'all)
(setq org-agenda-todo-ignore-with-date 'all)
(setq org-tags-column -80)
(setq org-agenda-tags-column -80)
;; custom org capture templates
(setq org-capture-templates
	  '(("i" "Inbox" entry (file+headline "~/org/todo.org" "Inbox")
		 "** TODO %?")
	    ("a" "ATLAS" entry (file+headline "~/org/atlas.org" "ATLAS")
		 "** TODO %?")))
;; allow refile across all org agenda files
(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))

;; ----------------------------------
;; PYTHON MODE
;; ----------------------------------

(add-hook 'python-mode-hook (lambda() (display-fill-column-indicator-mode)))
(add-hook 'python-mode-hook (lambda() (set-fill-column 100)))

;; ----------------------------------
;; YAML MODE
;; ----------------------------------

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-hook 'yaml-mode-hook
 '(lambda ()
   (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
(add-hook 'yaml-mode-hook (lambda() (display-fill-column-indicator-mode)))
(add-hook 'yaml-mode-hook (lambda() (set-fill-column 80)))

;; ----------------------------------
;; KEY BINDINGS
;; ----------------------------------

;; text editing shortcuts
(global-set-key (kbd "C-c g") 'goto-line)

;; org mode shortcuts available everywhere
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; ----------------------------------
;; CUSTOM EMACS STUFF
;; ----------------------------------

