;;; package --- Summary
;;; Commentary:
;; to load separate files as modules
;;; Code:
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
    rjsx-mode
    json-mode
    web-mode
	jedi
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
    tide
    company
    flycheck
    exec-path-from-shell
    ag
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

(load-user-file "helm.el")
(load-user-file "ido.el")

(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))

;; org mode
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; web mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; emmet
(add-hook 'web-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

;;web mode
(setq web-mode-markup-indent-offset 4)
(setq web-mode-css-indent-offset 4)
(setq web-mode-code-indent-offset 4)
(setq web-mode-enable-auto-pairing 4)

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js-mode-hook 'auto-complete-mode)
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" "2b5aa66b7d5be41b18cc67f3286ae664134b95ccc4a86c9339c886dfd736132d" default)))
 '(default-input-method "russian-computer")
 '(js-indent-level 4)
 '(js-switch-indent-offset 2)
 '(js2-basic-offset 4)
 '(js2-bounce-indent-p nil)
 '(js2-strict-missing-semi-warning nil)
 '(neo-click-changes-root t)
 '(neo-create-file-auto-open nil)
 '(neo-mode-line-type (quote neotree))
 '(neo-theme (quote nerd))
 '(org-babel-load-languages (quote ((python . t) (js . t) (sh . t) (emacs-lisp . t))))
 '(package-selected-packages
   (quote
    (rjsx-mode graphql-mode yaml-mode typing typit org nov exec-path-from-shell helm-ag iedit markdown-preview-eww 2048-game angular-mode smooth-scroll eslint-fix wgrep-ag writeroom-mode olivetti org-bullets autobookmarks smart-tab xah-find find-file-in-project fiplr ag package ob-translate picpocket js-auto-beautify color-theme-sanityinc-solarized solarized-theme html5-schema sass-mode company tide markdown-mode tomatinho ac-js2 ng2-mode magit kaomoji vue-mode helm-unicode dirtree neotree hackernews w3m powerline jsx-mode nodejs-repl flx-ido json-reformat json-mode js2-mode color-theme-solarized rubocop flymake-jshint flycheck ample-zen-theme color-theme-molokai color-theme ido-vertical-mode jedi emmet-mode js3-mode auto-complete)))
 '(yaml-indent-offset 4))

;; Vue.js
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
             
;; specific for Rails
(add-to-list 'auto-mode-alist '("\\.js.erb\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.css.erb\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.html.erb\\'" . web-mode))

;; python-mode
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)
;; (setq-default py-shell-name "ipython")
;; (setq-default py-which-bufname "IPython")

;; font settings
(set-default-font "Monaco")
(set-face-attribute 'default nil :height 140) 
(setq-default line-spacing 1)
;; linum
(linum-mode t)
(setq linum-format "%4d ")
(set-face-attribute 'linum nil :background "#000")


;; misc settings
(setq visible-bell 1)
(setq ring-bell-function 'ignore)

(powerline-default-theme)
(fset 'yes-or-no-p 'y-or-n-p)
(if (display-graphic-p)
    (normal-erase-is-backspace-mode 1))
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

;; stop creating backup and temp
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq create-lockfiles nil)

;; different themes if started in terminal or gui
(if (display-graphic-p) 
    (load-theme 'sanityinc-solarized-light) 
        (load-theme 'tangotango t))

;; (add-hook 'after-make-frame-functions
;;           (lambda (frame)
;;             (let ((mode (if (display-graphic-p frame) 'light 'dark)))
;;               (set-frame-parameter frame 'background-mode mode)
;;               (set-terminal-parameter frame 'background-mode mode))
;;             (enable-theme 'solarized)))

(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
;; globals
(load-user-file "globals.el")
;; different functions helpers
(load-user-file "helpers.el")
;; load individual modules
(load-user-file "keys.el")
;; sbcl
;;(load-user-file "clisp.el")
;; typescript
(load-user-file "typescript.el")

;; flycheck
;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)
;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)
;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))
;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)
;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))


(defun remove-newlines-in-region ()
  "Removes all newlines in the region."
  (interactive)
  (save-restriction
    (narrow-to-region (point) (mark))
    (goto-char (point-min))
    (while (search-forward "\n" nil t) (replace-match "" nil t))))

(global-set-key [f9] 'remove-newlines-in-region)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(defadvice js-jsx-indent-line (after js-jsx-indent-line-after-hack activate)
  "Workaround sgml-mode and follow airbnb component style."
  (save-excursion
    (beginning-of-line)
    (if (looking-at-p "^ +\/?> *$")
        (delete-char sgml-basic-offset))))
