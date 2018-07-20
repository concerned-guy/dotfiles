;;; init.el --- Emacs initialization file

;; Load Emacs Lisp packages and activate them
(package-initialize)

;; Global behavior
(setq-default tab-width 4           ;tab equals 4 spaces
              indent-tabs-mode nil  ;spaces instead of tabs
              vc-follow-symlinks t) ;follow links in link farms
;; Store backups in ~/.emacs.d
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/" t))
      backup-directory-alist '((".*" . "~/.emacs.d/backups/")))

;; Minor modes
(electric-indent-mode)
(electric-pair-mode)
(global-font-lock-mode -1)
(menu-bar-mode -1)
(recentf-mode)

;; Keybindings for comfortable editing experience
(global-set-key (kbd "C-c f") 'recentf-open-files)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "<f5>") 'smart-compile)

;; Package-specific settings
(setq c-default-style "stroustrup") ;c-mode

