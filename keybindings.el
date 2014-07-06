;;
;; All global changes to keybindings go in this section.
;;

;; Switch windows with one keystroke
(global-set-key (kbd "C-o") 'other-window)

;; Magit-status
(global-set-key (kbd "C-x C-g") 'magit-status)

;; Insert a dollar sign pair
(global-set-key (kbd "C-c C-d") 'insert-dollar-pair)

;; Enter into LaTeX display-math  mode
(global-set-key (kbd "C-c D") 'enter-display-math)

;; Hooks for html mode keybindings
(defun erik-html-mode-keys ()
  "Modify keymaps used by `html-mode'."
  (local-set-key (kbd "C-c C-z") 'insert-new-django-block)
)
(add-hook 'html-mode-hook 'erik-html-mode-keys)

(provide 'keybindings)
