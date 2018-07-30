;;; init.el --- emacs initialization file

;; Load emacs lisp packages and activate them
(package-initialize)

;; Global behavior
(setq-default tab-width 4           ;tab equals 4 spaces
              c-basic-offset 4      ;C language indentation
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

;; Keybindings
(global-set-key (kbd "M-/") 'hippie-expand)
