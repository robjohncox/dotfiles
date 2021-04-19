;; ----------------------------------
;; PACKAGE LIST
;; ----------------------------------

(setq package-list	
  '(python-mode yaml-mode markdown-mode dockerfile-mode docker-compose-mode))

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

;; org directory included automatically in agenda
(setq org-agenda-files (directory-files-recursively "~/org" "\\.org$"))
;; turn on auto-fill
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(setq org-agenda-span 2
      org-agenda-start-on-weekday nil
      org-agenda-start-day "-0d")
;; add alternate TODO workflows - by design we optimise for
;; TODO and DONE being the primary workflow, but want to also
;; allow more descriptive intermediate states when the need
;; arises.
(setq org-todo-keywords  '((sequence "TODO(t)" "|" "DONE(d)")
						   (sequence "LIVE(l)" "REVW(r)" "WAIT(w)" "SHAM(s)" "|")))
(setq org-todo-keyword-faces '(("LIVE" . "color-20")
							   ("REVW" . "color-202")
							   ("WAIT" . "color-214")
							   ("SHAM" . "color-214")))
;; all task progress logs put into a drawer
(setq org-log-into-drawer t)
;; hide scheduled tasks when looking at todos
(setq org-agenda-todo-ignore-scheduled 'all)
(setq org-agenda-todo-ignore-deadline 'all)
(setq org-agenda-todo-ignore-with-date 'all)
;; show tags next to heading
(setq org-tags-column -80)
(setq org-agenda-tags-column -80)
;; new task capture templates
(setq org-capture-templates
	  '(("i" "Inbox" entry (file+headline "~/org/gtd.org" "Inbox")
		 "** TODO %?")))
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
	("~/org/labminds.org" "~/org/health.org" "~/org/brown-bag.org" "~/org/gtd.org" "~/org/vacations.org" "~/org/tech.org" "~/org/recipes.org" "~/org/obs.org" "~/org/home.org" "~/org/finances.org" "~/org/career.org" "~/org/book.org"))))
 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
