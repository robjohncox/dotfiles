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
;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

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
(setq org-agenda-files '("~/org"))
;; turn on auto-fill
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; ----------------------------------
;; PYTHON MODE
;; ----------------------------------

(add-hook 'python-mode-hook (lambda() (display-fill-column-indicator-mode)))
(add-hook 'python-mode-hook (lambda() (set-fill-column 100)))

;; ----------------------------------
;; YAML MODE
;; ----------------------------------

(require 'yaml-mode)
7(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
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
 '(package-selected-packages '(markdown-mode dockerfile-mode docker-compose-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
