;; Mode specific hooks allow customization of behavior when a major
;; mode is loaded.

;; Global Hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; Mode Hooks
;;;;;;;;;;;;;

;; emacs-lisp
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;; rust
(add-hook 'rust-mode-hook 'cargo-minor-mode)

;; ruby
(add-hook 'ruby-mode-hook 'ruby-electric-mode)
(add-hook 'ruby-mode-hook (lambda () (auto-pair-mode -1)))

;; scheme
(add-hook 'scheme-mode-hook 'show-paren-mode)
(add-hook 'scheme-mode-hook 'paredit-mode)

;; term
(add-hook 'term-mode-hook (lambda () (yas-minor-mode -1)))

;; web
(defun my/web-mode-hook ()
  "Hooks for web mode"
  (setq web-mode-markup-indent-offset 2))
(add-hook 'web-mode-hook 'my/web-mode-hook)
(add-hook 'web-mode-hook (lambda () (autopair-mode -1)))

(provide 'hooks)
