
;; puppet mode
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

;; markdown mode
(add-to-list 'auto-mode-alist '("\\.[mM][dD]$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkdn$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd$" . markdown-mode))

(provide 'mode-mappings)
