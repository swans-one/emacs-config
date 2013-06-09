;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path site-lisp-dir)

;; Add the following subdirectories of the `site-lisp-dir`
(let ((default-directory site-lisp-dir))
  (normal-top-level-add-to-load-path
   '("dcpu16-mode" 
     "puppet-mode")
   ))

;; Require other modes
(require 'dcpu16-mode)
(require 'puppet-mode)

;;
;; Simple Customizations
;;

;; Ido Mode:
(require 'ido)
(ido-mode t)


;; Remove Tool Bar if graphical, menu bar otherwise
(if (display-graphic-p)
    (tool-bar-mode 0)
  (menu-bar-mode 0))

;; Visible Bell instead of Audible bell
(setq visible-bell 1)

;; Transient Mark Mode
(transient-mark-mode t)

;; NEVER EVER USE TABS
(setq indent-tabs-mode nil)

;; Save all autosave files in a backups directory
(defvar user-temporary-file-directory 
"~/.emacs-backup")
(setq backup-directory-alist
      `((".*" . ,user-temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

