;;
;; All global changes to keybindings go in this section.
;;

;; Switch windows with one keystroke
(global-set-key (kbd "C-o") 'other-window)

;; Find file at point
(global-set-key (kbd "C-c C-f") 'find-file-at-point)

;; Magit-status
(global-set-key (kbd "C-x C-g") 'magit-status)

;; Electric-Newline-And-Maybe-Indent
(global-set-key (kbd "<C-return>") 'electric-newline-and-maybe-indent)

;; Insert a dollar sign pair
(global-set-key (kbd "C-c C-d") 'insert-dollar-pair)

;; Enter into LaTeX display-math  mode
(global-set-key (kbd "C-c D") 'enter-display-math)

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

;; Compile with ./build.sh
(global-set-key (kbd "C-c b") 'compile-build-sh)

;; Make an echoing shell behave correctly
(global-set-key (kbd "C-c e") 'shell-echoes)

;; unpop-to-mark
(global-set-key (kbd "C-x C-p") 'unpop-to-mark-command)

;; expand-region
(global-set-key (kbd "C-]") 'er/expand-region)

;; collapse-line-back
(global-set-key (kbd "<C-M-backspace>") 'collapse-line-back)

;; toggle-theme
(global-set-key (kbd "C-x C-h") 'toggle-theme)

;; Rotate mode
(global-set-key (kbd "C-x C-m C-m") 'rotate-mode)

;;
;; Mode Specific Keybindings
;;

;; Hooks for html mode keybindings
(defun erik-html-mode-keys ()
  "Modify keymaps used by `html-mode'."
  (local-set-key (kbd "C-c C-z") 'insert-new-django-block)
)
(add-hook 'html-mode-hook 'erik-html-mode-keys)

;; Hooks for python mode keybindings
(defun erik-python-mode-keys ()
  "Modify kymaps used by `python-mode'."
  (local-set-key (kbd "C-c C-w") 'python-what-class)
)
(add-hook 'python-mode-hook 'erik-python-mode-keys)


(provide 'keybindings)
