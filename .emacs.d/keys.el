;; common
;; key bindings
(windmove-default-keybindings 'meta) 

(global-set-key [f1] 'ibuffer)
(global-set-key [f2] 'neotree-toggle)
(global-set-key [f3] 'ansi-term)
(global-set-key [f8] 'remove-dos-eol)

;; helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-o") 'helm-occur)

;; projectile
(projectile-global-mode)
(global-set-key (kbd "C-x f") 'projectile-find-file)

