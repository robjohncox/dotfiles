;; show line numbers on each line
(global-display-line-numbers-mode)
;; lines are visually wrapped
(global-visual-line-mode)
;; use spaces instead of tabs
(setq-default indent-tabs-mode 0)

(load-theme 'whiteboard)

;; always open web links in emacs browser
(setq browse-url-browser-function 'eww-browse-url)

;; org directory included automatically in agenda
(setq org-agenda-files '("~/org"))

;; org mode shortcuts available everywhere
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

