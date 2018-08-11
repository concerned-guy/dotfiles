;;; init.el --- emacs initialization file

;; Global behavior
(setq auto-save-default nil)          ;disable autosaves
(setq auto-save-list-file-prefix nil) ;disable listing of autosaves
(setq make-backup-files nil)          ;disable backups
(setq-default c-basic-offset 4)	      ;style variable
(setq-default indent-tabs-mode nil)   ;spaces instead of tabs

;; Minor modes
(global-font-lock-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
