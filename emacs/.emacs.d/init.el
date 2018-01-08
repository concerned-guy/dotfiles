;;; init.el --- Emacs Initialization File

;; ==================================================
;; Repositories
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

;; ==================================================
;; Globals
(setq-default tab-width 4 ;; tab equals 4 spaces
              indent-tabs-mode nil ;; spaces instead of tabs
              require-final-newline t) ;; newline before EOF
;; backup in emacs dir
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/" t))
      backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
;; theme
(load-theme 'zenburn t)

;; ==================================================
;; Modes
(column-number-mode)
(electric-pair-mode)
(global-auto-revert-mode)
(global-company-mode)
(global-nlinum-mode)
(global-whitespace-cleanup-mode)
(ido-mode)
(ido-everywhere)
(ido-ubiquitous-mode)
(menu-bar-mode -1)
(nyan-mode)
(projectile-mode)
(recentf-mode)
(smex-initialize)
(show-paren-mode)
(whole-line-or-region-mode)
(xclip-mode)
(yas-global-mode)

;; ==================================================
;; Keys

;; custom
(global-set-key (kbd "C-c f") 'recentf-open-files)
(global-set-key (kbd "C-M-@") 'er/expand-region)
(global-set-key (kbd "M-;") 'comment-dwim-2)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-+") 'shift-number-up)
(global-set-key (kbd "M-_") 'shift-number-down)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "<f5>") 'smart-compile)

;; newline/space at bracket
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-j") 'newline-and-indent)
(global-set-key (kbd "SPC") 'electric-space)
(defun electric-space ()
  "Proper space for brackets."
  (interactive)
  (let ((open-delims (or (and (looking-back "(" 1) (looking-at ")"))
                         (and (looking-back "{" 1) (looking-at "}"))
                         (and (looking-back "<" 1) (looking-at ">"))
                         (and (looking-back "\\[" 1) (looking-at "\\]")))))
    (insert " ")
    (when open-delims (save-excursion (insert " ")))))

;; ==================================================
;; Packages

;; asm-mode
(add-hook 'asm-mode-hook (lambda() (setq comment-start "#")))

;; c-mode
(setq c-default-style "stroustrup")
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))
(add-hook 'c++-mode-hook (lambda () (setq c-basic-offset 2)))

;; ggtags-mode
(add-hook 'c-mode-common-hook 'ggtags-mode) ;; c, c++, java
(add-hook 'python-mode-hook 'ggtags-mode) ;; python

;; projectile-mode
(setq projectile-switch-project-action 'projectile-dired)
(add-to-list 'projectile-other-file-alist '("ino" "h" "hpp" "ipp"))
(add-to-list 'projectile-other-file-alist '("h" "ino" "c" "cc" "cpp" "ipp" "hpp" "cxx" "ixx" "hxx" "m" "mm"))
(add-to-list 'projectile-project-root-files-bottom-up "platformio.ini")
(projectile-register-project-type 'platformio
                                  '("platformio.ini")
                                  :compile "platformio run"
                                  :run "pkill dterm; platformio run --target upload")

;; emmet-mode
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

;; web-mode
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; org-mode
(setq org-blank-before-new-entry nil
      org-startup-folded nil
      org-startup-indented t)

;; recentf-mode
(setq recentf-auto-cleanup 'never
      recentf-max-saved-items 200)

;; smart-compile
(require 'smart-compile)
(setq compilation-read-command nil)
(add-to-list 'smart-compile-alist '(asm-mode . "spim -f %f"))

;; compilation-mode
(setq compilation-scroll-output t)

;; rainbow-delimiter-mode
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; ======================================================
;; Customize (M-x customize RET)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (mips-mode smex projectile ggtags web-mode zenburn-theme ido-completing-read+ matlab-mode xclip company markdown-mode nlinum rainbow-delimiters nyan-mode shift-number whole-line-or-region smart-compile emmet-mode comment-dwim-2 yasnippet whitespace-cleanup-mode expand-region))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
