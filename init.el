;; This file is broken up into two sections.
;;
;; The first section contains simple customizations, which make sense
;; to include in this init file. These are customizations that I want
;; to make sure are loaded, even if something later on fails.
;;
;; The second section contains instructions to load other modules and
;; bits of code, which make more drastic changes to the
;; environment. This includes loading other packages and major modes.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        Simple Customizations         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ido Mode:
(require 'ido)
(ido-mode t)
;; Remove backwards searching in path for file
(setq ido-auto-merge-work-directories-length -1)

;; Remove Tool Bar if graphical, menu bar otherwise
(if (display-graphic-p)
    (tool-bar-mode 0)
  (menu-bar-mode 0))

;; Visible Bell instead of Audible bell
(defun my-terminal-visible-bell ()
   "A friendlier visual bell effect."
   (invert-face 'mode-line)
   (run-with-timer 0.1 nil 'invert-face 'mode-line))
(setq visible-bell nil ring-bell-function 'my-terminal-visible-bell)

;; Transient Mark Mode
(transient-mark-mode t)

;; show column number in mode line
(setq column-number-mode t)

;; NEVER EVER USE TABS
(setq-default indent-tabs-mode nil)

;; Save all autosave files in a backups directory
(defvar user-temporary-file-directory
"~/.emacs-backup")
(setq backup-directory-alist
      `((".*" . ,user-temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

;; Change magit's colors to something sensible
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red")
     (when (not window-system)
       (set-face-background 'magit-item-highlight "black"))))

;; Better unique names for similarly named buffers
;;
;; post-forward mode adds additional path information after a | after
;; the filenames of similarly named files.
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; Use pdflatex to compile LaTeX files
(setq latex-run-command "pdflatex")

;; Custom options for `C-u M-x ps-print-buffer-with-faces`
(setq ps-print-header nil)

;; C-mode customizations
(setq-default c-default-style "linux"
              c-basic-offset 4)

;; Use subword mode everywhere. Makes camelCase into separate words.
(global-subword-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         Loading Other Elisp          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path site-lisp-dir)

;; IMPORTANT: import all my defuns
(require 'defuns)

;; Add the following subdirectories of the `site-lisp-dir` to the
;; load-path.
(let ((default-directory site-lisp-dir))
  (normal-top-level-add-to-load-path
   '("dcpu16-mode"
     "puppet-mode"
     "markdown-mode"
     "rainbow-delimiters")
   ))


;; Require other modes
(require 'dcpu16-mode)
(require 'puppet-mode)
(require 'markdown-mode)
(require 'rainbow-delimiters)

;; ESS
; If this fails install ESS:
;     cd && mkdir .ESS && cd .ESS
;     git clone git://github.com/emacs-ess/ESS.git && cd ESS && make all
(load "~/.ESS/ESS/lisp/ess-site")

;;
;; Package Management
;;

;; Add other repositories to the package manager
(require 'package)
(add-to-list
 'package-archives
 '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Which package to make sure are installed
(setq keep-installed '(magit paredit yasnippet geiser quack))

;; If any of `keep-installed` are not installed, install them

(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it?" package))
	   (package-install package))))
 keep-installed)


;; YaSnippet
(require 'yasnippet)
(setq yas-snippetdirs
      '("~/.emacs.d/snippets"))
(yas-global-mode 1)
(setq yas-prompt-functions '(yas-ido-prompt))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         Mode Specific Hooks          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'scheme-mode-hook 'show-paren-mode)
(add-hook 'scheme-mode-hook 'paredit-mode)
;; (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)

(add-hook 'emacs-lisp-mode 'show-paren-mode)
(add-hook 'emacs-lisp-mode 'paredit-mode)



;;
;; Mode Mappings
;;
(require 'mode-mappings)
(require 'keybindings)

(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-mode-hook (quote (turn-on-haskell-indent)))
 '(rainbow-delimiters-max-face-count 4)
 '(rainbow-delimiters-outermost-only-face-count 1))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-item-highlight ((t nil)))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "green"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "magenta"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "blue"))))
 '(rst-level-1 ((t (:background "black"))))
 '(rst-level-2 ((t (:background "color-17"))))
 '(rst-level-3 ((t nil))))
(put 'upcase-region 'disabled nil)
