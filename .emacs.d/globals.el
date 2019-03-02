;; globals
;(global-auto-complete-mode t)
(global-linum-mode)
(menu-bar-mode -1)

;; highlighted line
(global-hl-line-mode 1)
(setq global-visual-line-mode t)
(global-prettify-symbols-mode)
(nyan-mode 1)

;; linum
(linum-mode t)
(setq linum-format "%4d ")
(set-face-attribute 'linum nil :background "#000")

;; misc settings
(setq visible-bell 1)
(setq ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)
(if (display-graphic-p)
    (normal-erase-is-backspace-mode 1))
(setq ns-right-alternate-modifier nil)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(setq scroll-step 1) 

;; stop creating backup and temp
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq create-lockfiles nil)

