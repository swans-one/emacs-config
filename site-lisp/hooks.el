;; Mode specific hooks allow customization of behavior when a major
;; mode is loaded.

;; Global Hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; Mode Hooks
;;;;;;;;;;;;;

;; emacs-lisp
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

;; scheme
(add-hook 'scheme-mode-hook 'show-paren-mode)

;; term
(add-hook 'term-mode-hook (lambda () (yas-minor-mode -1)))

;; web
(defun my/set-php-method-key ()
  (when (equal web-mode-engine "php")
    (local-set-key (kbd ">") (lambda () (interactive) (insert "->")))
    (local-set-key (kbd "C->") (lambda () (interactive) (insert "=>")))))
(defun my/web-mode-hook ()
  "Hooks for web mode"
  (setq web-mode-markup-indent-offset 2)
  (my/set-php-method-key))
(add-hook 'web-mode-hook 'my/web-mode-hook)
(add-hook 'web-mode-hook (lambda () (autopair-mode -1)))


(provide 'hooks)
