;;
;; All global changes to keybindings go in this section.
;;

;; Builtins re-bindings
;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-c C-f") 'find-file-at-point)
(global-set-key (kbd "<C-return>") 'electric-newline-and-maybe-indent)

;; Package rebindings
;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-x C-g") 'magit-status)
(global-set-key (kbd "C-]") 'er/expand-region)


;; My function definitions (from ./defuns.el)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-x ,") 'my/wrap-line)
(global-set-key (kbd "C-x i") 'my/wrap-line-indent)
(global-set-key (kbd "C-M-c") 'my/run-commented)
(global-set-key (kbd "C-c u") 'my/underline-line)
(global-set-key (kbd "C-c C-u") 'my/underline-line)
(global-set-key (kbd "M-Q") 'my/unfill-paragraph)
(global-set-key (kbd "C-c b") 'my/compile-build-sh)
(global-set-key (kbd "C-c e") 'my/shell-echoes)
(global-set-key (kbd "C-x C-p") 'my/unpop-to-mark-command)
(global-set-key (kbd "<C-M-backspace>") 'my/collapse-line-back)
(global-set-key (kbd "C-x C-h") 'my/toggle-theme)
(global-set-key (kbd "C-x C-m C-m") 'my/rotate-mode)


;; Mode Specific Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun erik-html-mode-keys ()
  "Modify keymaps used by `html-mode'."
  (local-set-key (kbd "C-c C-z") 'insert-new-django-block)
)
(add-hook 'html-mode-hook 'erik-html-mode-keys)

(defun erik-python-mode-keys ()
  "Modify kymaps used by `python-mode'."
  (local-set-key (kbd "C-c C-w") 'python-what-class)
)
(add-hook 'python-mode-hook 'erik-python-mode-keys)


(provide 'keybindings)
