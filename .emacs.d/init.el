(require 'cl)
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(global-auto-complete-mode t)

(ido-mode 1)
(ido-vertical-mode 1)

(autopair-global-mode)

;; emmet
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

;; web mode
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(setq web-mode-engines-alist
      '(("django"    . "\\.html\\'")
	))
;; js3-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js3-mode))

;; font settings
(set-default-font "Consolas")
(set-face-attribute 'default nil :height 110) 
(setq-default line-spacing 1)
;; linum
(linum-mode t)
(setq linum-format "%4d ")
(set-face-attribute 'linum nil :background "#000")
(global-linum-mode)

;; highlighted line
(defface hl-line '((t (:background "DarkSlateGrey")))
  "Face to use for `hl-line-face'." :group 'hl-line)
(setq hl-line-face 'hl-line)
(global-hl-line-mode 1)
(setq global-visual-line-mode t)

;; misc settings
(fset 'yes-or-no-p 'y-or-n-p)
;(normal-erase-is-backspace-mode 1)
(mouse-wheel-mode t)
(windmove-default-keybindings 'meta) 
(scroll-bar-mode -1)
(menu-bar-mode -99)

(setq scroll-step 1) 
(setq mac-option-modifier 'none)
;; tabs
(setq-default tab-width 4)
(setq-default indent-tabs-mode t)

;;autofill disable
(auto-fill-mode -1)

;; key bindings
(global-set-key [f1] 'ibuffer)

(color-theme-molokai)
;(load-theme 'tsdh-dark)
