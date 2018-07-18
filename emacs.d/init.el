;;; init.el --- Emacs Initialization File

;; ==================================================
;; Repositories

(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

;; ==================================================
;; Globals

(setq-default tab-width 4             ;; tab equals 4 spaces
              indent-tabs-mode nil    ;; spaces instead of tabs
              require-final-newline t ;; newline before EOF
              vc-follow-symlinks t)   ;; follow links in link farms
;; backup in ~/.emacs.d
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/" t))
      backup-directory-alist '((".*" . "~/.emacs.d/backups/")))

;; ==================================================
;; Modes

(electric-pair-mode)
(global-font-lock-mode -1)
(global-whitespace-cleanup-mode)
(menu-bar-mode -1)
(recentf-mode)
(xclip-mode)

;; ==================================================
;; Keys

;; comfortable editing
(global-set-key (kbd "C-c f") 'recentf-open-files)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "<f5>") 'smart-compile)

;; newline/space at brackets
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-j") 'newline-and-indent)

;; ==================================================
;; Packages

;; c-mode
(setq c-default-style "stroustrup")

;; org-mode
(setq org-blank-before-new-entry nil
      org-startup-folded nil
      org-startup-indented t)

;; recentf-mode
(setq recentf-auto-cleanup 'never
      recentf-max-saved-items 200)

;; compilation-mode
(setq compilation-read-command nil
      compilation-scroll-output t
      compilation-window-height 12)

;; ======================================================
;; Customize (M-x customize RET)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (xclip smart-compile whitespace-cleanup-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
