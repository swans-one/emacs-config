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

;; Wrapping a single line
(global-set-key (kbd "C-x ,") 'wrap-line)
(global-set-key (kbd "C-x i") 'wrap-line-indent)

;; Execute a commented region
(global-set-key (kbd "C-M-c") 'run-commented)

;; Underline a line
(global-set-key (kbd "C-c u") 'underline-line)
(global-set-key (kbd "C-c C-u") 'underline-line)

;; Unfill paragraph
(global-set-key (kbd "M-Q") 'unfill-paragraph)

(provide 'keybindings)
