;; Web Mode / JSX mode
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;; (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))

;; (setq web-mode-content-types-alist
;;       '(("jsx" . "\\.js[x]?\\'")))

;; (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
;; (setq web-mode-engines-alist
;;   '(("php"    . "\\.phtml\\'")
;;     ("blade"  . "\\.blade\\."))
;; )

;; puppet mode
;; (add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode)) ;

;; markdown mode
(add-to-list 'auto-mode-alist '("\\.[mM][dD]$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkdn$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd$" . markdown-mode))

;; octave mode
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

(provide 'mode-mappings)
