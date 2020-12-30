;; ----------------------------------
;; PACKAGES
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

;; ----------------------------------
;; GENERAL
;; ----------------------------------

;; theme
(load-theme 'whiteboard)
;; follow symlinks automatically when opening files
(setq vc-follow-symlinks t)

;; ----------------------------------
;; TEXT EDITING
;; ----------------------------------

;; default tab width
(setq-default tab-width 4)
;; use spaces instead of tabs
(setq-default indent-tabs-mode 0)
;; make text mode the default major mode
(setq default-major-mode 'text-mode)
;; turn on auto-fill automatically in various modes
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

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

;; ----------------------------------
;; YAML MODE
;; ----------------------------------

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-hook 'yaml-mode-hook
 '(lambda ()
   (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; ----------------------------------
;; MARKDOWN MODE
;; ----------------------------------

(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))

;; ----------------------------------
;; MARKDOWN MODE
;; ----------------------------------

(unless (package-installed-p 'dockerfile-mode)
  (package-install 'dockerfile-mode))
(unless (package-installed-p 'docker-compose-mode)
  (package-install 'docker-compose-mode))

;; ----------------------------------
;; PROJECTILE
;; ----------------------------------

(unless (package-installed-p 'projectile)
  (package-install 'projectile))

;; ----------------------------------
;; KEY BINDINGS
;; ----------------------------------

;; text editing shortcuts
(global-set-key (kbd "C-c g") 'goto-line)

;; org mode shortcuts available everywhere
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; projectile shortcuts
(global-set-key (kbd "C-c p") 'projectile-command-map)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (yaml-mode markdown-mode projectile))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
