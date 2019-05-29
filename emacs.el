;;; emacs.el --- emacs initialization file

(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(setq auto-save-default nil)            ;disable autosaves
(setq auto-save-list-file-prefix nil)   ;disable listing of autosaves
(setq default-frame-alist '((vertical-scroll-bars) (tool-bar-lines . 0)))
(setq inhibit-startup-message t)        ;disable startup screen
(setq make-backup-files nil)            ;disable backups
(setq-default c-basic-offset 4)         ;style variable
(setq-default indent-tabs-mode nil)     ;spaces instead of tabs
