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
              require-final-newline t ;; newline before EOF
              vc-follow-symlinks t) ;; follow links in link farms
;; backup in ~/.emacs.d
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/" t))
      backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
;; random theme
(setq user-themes '(sanityinc-tomorrow-eighties))
(load-theme (nth (random (length user-themes)) user-themes) t)

;; ==================================================
;; Modes

(column-number-mode)
(electric-pair-mode)
(global-company-mode)
(global-nlinum-mode)
(global-whitespace-cleanup-mode)
(ido-mode)
(ido-everywhere)
(ido-ubiquitous-mode)
(menu-bar-mode -1)
(projectile-mode)
(recentf-mode)
(smex-initialize)
(whole-line-or-region-mode)
(xclip-mode)
(yas-global-mode)

;; ==================================================
;; Keys

(global-set-key (kbd "C-c f") 'recentf-open-files)
(global-set-key (kbd "C-M-@") 'er/expand-region)
(global-set-key (kbd "M-;") 'comment-dwim-2)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-+") 'shift-number-up)
(global-set-key (kbd "M-_") 'shift-number-down)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "<f5>") 'smart-compile)
;; newline/space at brackets
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

;; verilog-mode
(setq verilog-linter "iverilog")
(add-to-list 'auto-mode-alist '("\\.xdc\\'" . conf-mode))

;; ggtags-mode
(add-hook 'c-mode-common-hook 'ggtags-mode) ;; c, c++, java
(add-hook 'python-mode-hook 'ggtags-mode) ;; python

;; octave-mode
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; emmet-mode
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'web-mode-hook 'emmet-mode) ;; Auto-start on web mode

;; web-mode
(setq web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-code-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; js-mode
(setq js-indent-level 2)

;; org-mode
(setq org-blank-before-new-entry nil
      org-startup-folded nil
      org-startup-indented t)

;; recentf-mode
(setq recentf-auto-cleanup 'never
      recentf-max-saved-items 200)

;; compilation-mode
(setq-default compilation-read-command nil
              compilation-scroll-output t)

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
    (xclip smex projectile ggtags web-mode ido-completing-read+ markdown-mode nlinum rainbow-delimiters shift-number whole-line-or-region smart-compile emmet-mode comment-dwim-2 whitespace-cleanup-mode expand-region))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
