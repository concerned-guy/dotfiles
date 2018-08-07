;;; init.el --- emacs initialization file

;; Global behavior
(setq-default tab-width 4               ;tab equals 4 spaces
              c-basic-offset 4          ;C language indentation
              indent-tabs-mode nil      ;spaces instead of tabs
              auto-save-default nil     ;disable autosaves
              make-backup-files nil)    ;disable backups

;; Minor modes
(global-font-lock-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)