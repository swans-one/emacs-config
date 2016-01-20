;; Mode specific hooks allow customization of behavior when a major
;; mode is loaded.

(add-hook 'scheme-mode-hook 'show-paren-mode)
(add-hook 'scheme-mode-hook 'paredit-mode)

(add-hook 'emacs-lisp-mode 'show-paren-mode)
(add-hook 'emacs-lisp-mode 'paredit-mode)

(provide 'hooks)
