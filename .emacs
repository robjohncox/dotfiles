;; ----------------------------------
;; GENERAL
;; ----------------------------------

;; theme
(load-theme 'whiteboard)
;; show line numbers on each line
(global-display-line-numbers-mode)
;; show column number in status bar
(setq column-number-mode t)
;; follow symlinks automatically when opening files
(setq vc-follow-symlinks t)

;; ----------------------------------
;; TEXT EDITING
;; ----------------------------------

;; use spaces instead of tabs
(setq-default indent-tabs-mode 0)
;; make text mode the default major mode
(setq default-major-mode 'text-mode)
;; turn on auto-fill automatically in various modes
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

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
;; KEY BINDINGS
;; ----------------------------------

;; text editing shortcuts
(global-set-key (kbd "C-c g") 'goto-line)

;; org mode shortcuts available everywhere
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

