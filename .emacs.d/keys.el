;; common
;; key bindings
;;; Code:
(windmove-default-keybindings 'meta) 

(global-set-key [f1] 'ibuffer)
(global-set-key [f2] 'neotree-toggle)
(global-set-key [f3] 'ansi-term)
(global-set-key [f4] 'slime)
(global-set-key [f8] 'remove-dos-eol)

;; helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-o") 'helm-occur)
(global-set-key (kbd "C-x C-g") 'helm-projectile-grep)
;; projectile
(projectile-global-mode)
(global-set-key (kbd "C-x f") 'projectile-find-file)
(global-set-key (kbd "C-x p") 'projectile-switch-project)

