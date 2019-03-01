;;; package --- Summary
;;; Commentary:
;; key bindings
;;; Code:
(windmove-default-keybindings 'meta)

(global-set-key [f1] 'helm-mini)
(global-set-key [f2] 'neotree-toggle)
(global-set-key [f3] 'ansi-term)

(global-set-key [f5] 'rjsx-mode)
(global-set-key [f6] 'whitespace-mode)
(global-set-key [f8] 'remove-dos-eol)
(global-set-key [f9] 'remove-newlines-in-region)

;; helm
(global-set-key (kbd "M-x") 'helm-M-x)
(provide 'keys)
;;; keys.el ends here
