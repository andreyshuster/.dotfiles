(require 'cl)
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar required-packages
  '(
    auto-complete
    js3-mode
    web-mode
    emmet-mode
	jedi
    ido-ubiquitous
    ido-vertical-mode
    color-theme
    color-theme-molokai
	ample-zen-theme
    ) "a list of packages to install")
; method to check if all packages are installed
(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

; if not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
					; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
					; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(global-auto-complete-mode t)

(ido-mode 1)
(ido-vertical-mode 1)

;; web mode
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(setq web-mode-engines-alist
      '(("django"    . "\\.html\\'")
	))

;; emmet
(add-hook 'web-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

;;web mode
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-enable-auto-pairing t)

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js-mode-hook 'auto-complete-mode)
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; js3-mode
;(add-to-list 'auto-mode-alist '("\\.js\\'" . js3-mode))
;(add-hook 'js3-mode-hook 'auto-complete-mode)

;; python-mode
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")

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
(global-hl-line-mode 1)
(setq global-visual-line-mode t)

;; misc settings
(fset 'yes-or-no-p 'y-or-n-p)
(normal-erase-is-backspace-mode 1)
(setq ns-right-alternate-modifier nil)
;(mouse-wheel-mode t)
(windmove-default-keybindings 'meta) 
;(scroll-bar-mode -1)
;; Remove scrollbars, menu bars, and toolbars
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq scroll-step 1) 
(setq mac-option-modifier 'none)
;; tabs
(setq-default tab-width 4)
(setq-default indent-tabs-mode t)

;;autofill disable
(auto-fill-mode -1)

;; key bindings
(global-set-key [f1] 'ibuffer)

;(color-theme-molokai)
;(load-theme 'tsdh-dark)
;(load-theme 'tango)
(load-theme 'ample-zen)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
	("1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" "2b5aa66b7d5be41b18cc67f3286ae664134b95ccc4a86c9339c886dfd736132d" default)))
 '(js2-basic-offset 2)
 '(package-selected-packages
   (quote
	(flx-ido json-reformat json-mode js2-mode color-theme-solarized rubocop flymake-jshint flycheck ample-zen-theme color-theme-molokai color-theme ido-vertical-mode ido-ubiquitous jedi emmet-mode web-mode js3-mode auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
