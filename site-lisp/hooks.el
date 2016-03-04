;; Mode specific hooks allow customization of behavior when a major
;; mode is loaded.

(add-hook 'scheme-mode-hook 'show-paren-mode)
(add-hook 'scheme-mode-hook 'paredit-mode)

(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

(add-hook 'term-mode-hook (lambda () (yas-minor-mode -1)))

(provide 'hooks)
