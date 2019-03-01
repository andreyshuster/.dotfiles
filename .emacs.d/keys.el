;;; package --- Summary
;;; Commentary:
;; key bindings
;;; Code:
(windmove-default-keybindings 'meta)

(global-set-key [f1] 'ibuffer)
(global-set-key [f2] 'neotree-toggle)
(global-set-key [f3] 'ansi-term)

(global-set-key [f5] 'rjsx-mode)
(global-set-key [f6] 'whitespace-mode)
(global-set-key [f7] 'ag)
(global-set-key [f8] 'remove-dos-eol)
(global-set-key [f9] 'remove-newlines-in-region)

(global-set-key [f12] 'tomatinho)
;; helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-o") 'helm-occur)
(global-set-key (kbd "C-x C-g") 'helm-projectile-grep)
;; projectile
(projectile-mode)
(global-set-key (kbd "C-x f") 'projectile-find-file)
(global-set-key (kbd "C-x p") 'projectile-switch-project)
(provide 'keys)
;;; keys.el ends here
