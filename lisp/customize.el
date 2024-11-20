;; Get rid of visual cruft
(when (display-graphic-p)
  (tool-bar-mode -1)
  (menu-bar-mode -1))
(scroll-bar-mode -1)
(setq column-number-mode t)
(setq inhibit-startup-message t)

;; Editing improvements: transient-mark, tabs, subword mode, delete whitespace
(transient-mark-mode t)
(setq-default
 indent-tabs-mode nil
 tab-stop-list (quote (4 8 12))
 tab-width 4
 indent-line-function 'insert-tab)
(global-subword-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(electric-indent-mode 1)

;; Make the text size reasonable
(set-face-attribute 'default nil :height 120)

;; Visible Bell instead of Audible bell
(defun my-terminal-visible-bell ()
   "A friendlier visual bell effect."
   (invert-face 'mode-line)
   (run-with-timer 0.1 nil 'invert-face 'mode-line))
(setq visible-bell nil ring-bell-function 'my-terminal-visible-bell)

;; Better scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 8) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; Save all autosave files in a backups directory
(defvar user-temporary-file-directory
  "~/.emacs-backup")
(setq backup-directory-alist
      `((".*" . ,user-temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))
(setq auto-save-list-file-prefix user-temporary-file-directory)

;; Custom options for `C-u M-x ps-print-buffer-with-faces`
(setq ps-print-header nil)

;; Load Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'ample-zen t)


;; Customize Options (defcustom / defvar)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom default-dark-color-theme 'ample-zen
  "Default dark color-scheme")
(defcustom default-light-color-theme 'whiteboard
  "Default light color-scheme")


(provide 'customize)
