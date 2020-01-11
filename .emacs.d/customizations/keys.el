;;; package --- Summary
;;; Commentary:
;; key bindings
;;; Code:
(windmove-default-keybindings 'meta)

(global-set-key [f1] 'ibuffer)
(global-set-key [f2] 'neotree-projectile-action)
(global-set-key [f3] 'ansi-term)

(global-set-key [f4] 'paredit-mode)
(global-set-key [f6] 'whitespace-mode)
(global-set-key [f7] 'ag)
(global-set-key [f8] 'remove-dos-eol)
(global-set-key [f9] 'remove-newlines-in-region)

;; mouse scroll in terminal
(xterm-mouse-mode 1)
(define-key key-translation-map (kbd "<s-mouse-1>") (kbd "<mouse-2>"))
(global-set-key [mouse-4] 'scroll-down-line)
(global-set-key [mouse-5] 'scroll-up-line)

;; helm
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x f") 'projectile-find-file)
(global-set-key (kbd "C-x p") 'projectile-switch-project)
(global-set-key (kbd "C-s") 'helm-occur)

;; dumb-jump-go
(global-set-key (kbd "C-x g") 'dumb-jump-go)

(provide 'keys)
;;; keys.el ends here

