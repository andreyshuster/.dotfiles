;; to load all modules
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

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
    js2-mode
    jsx-mode
    json-mode
    web-mode
    emmet-mode
	jedi
    ido-ubiquitous
    ido-vertical-mode
    helm
    projectile
    helm-projectile
    color-theme
    color-theme-molokai
    tangotango-theme
    color-theme-solarized
    powerline
    neotree
    nyan-mode
    rainbow-mode
    ) "a list of packages to install")

;; method to check if all packages are installed
(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

;; if not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(global-auto-complete-mode t)

(load-user-file "helm.el")
(load-user-file "ido.el")

;; web mode
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))

;; emmet
(add-hook 'web-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

;; rainbow
(add-hook 'css-mode-hook 'rainbow-mode)

;;web mode
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-enable-auto-pairing t)

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js-mode-hook 'auto-complete-mode)
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; specific for Rails
(add-to-list 'auto-mode-alist '("\\.js.erb\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.css.erb\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.html.erb\\'" . web-mode))

;; python-mode
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")

;; font settings
(set-default-font "Monaco")
(set-face-attribute 'default nil :height 150) 
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
(setq visible-bell 1)
;;(setq ring-bell-function 'ignore)

(powerline-default-theme)
(fset 'yes-or-no-p 'y-or-n-p)
(normal-erase-is-backspace-mode 1)
(setq ns-right-alternate-modifier nil)
;(mouse-wheel-mode t)
;(scroll-bar-mode -1)
;; Remove scrollbars, menu bars, and toolbars
;;(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;;(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq scroll-step 1) 
;(setq mac-option-modifier 'none)
;; tabs
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;;autofill disable
(auto-fill-mode -1)

;; autocomplete
(ac-config-default)

(global-prettify-symbols-mode)

(if (display-graphic-p) 
      (load-theme 'tangotango t) ;sanityinc-solarized-light) 
        (load-theme 'tangotango t))

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; load individual modules
(load-user-file "keys.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" "2b5aa66b7d5be41b18cc67f3286ae664134b95ccc4a86c9339c886dfd736132d" default)))
 '(default-input-method "russian-computer")
 '(neo-click-changes-root t)
 '(neo-create-file-auto-open nil)
 '(neo-mode-line-type (quote neotree))
 '(neo-theme (quote nerd))
 '(package-selected-packages
   (quote
    (dirtree neotree hackernews w3m powerline jsx-mode nodejs-repl flx-ido json-reformat json-mode js2-mode color-theme-solarized rubocop flymake-jshint flycheck ample-zen-theme color-theme-molokai color-theme ido-vertical-mode ido-ubiquitous jedi emmet-mode web-mode js3-mode auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
