;; init.el is my entry point for emacs customization. This file should
;; be used exclusively for setting up the load path, handling
;; packages, and requiring other files that contain the actual
;; customizations that I'm including.
;;
;; The customizations are further organized into the following files,
;; all within `site-lisp`:
;;
;;     - customize.el   = customizations to the base system
;;     - defuns.el      = functions that I've written
;;     - keybindings.el = Global and mode specific keybindings
;;     - mode-mappings  = map filenames to major modes
;;     - hooks          = load elisp on major mode loading


;; Load Path
;;;;;;;;;;;;

(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path site-lisp-dir)


;; Package Management
;;;;;;;;;;;;;;;;;;;;;

;; Add other repositories to the package manager
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; Requires
;;;;;;;;;;;
(require 'customize)
(require 'defuns)
(require 'keybindings)
(require 'mode-mappings)
(require 'hooks)
(require 'cust-org)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ack-executable "/Users/erik/bin/ack")
 '(custom-safe-themes
   (quote
    ("9494d6d64290602054292f7c1b2db4285b3fea4fbf63b54bdac21aa6f6b0a7e6" "f897f31a459baa86363c91ab0d98d184e41d42fd2c33ec39e72561f25bd8138b" default)))
 '(electric-indent-mode nil)
 '(package-selected-packages
   (quote
    (use-package gnu-elpa-keyring-update yaml-mode emmet-mode web-mode yasnippet quack paredit magit haskell-mode geiser expand-region dash-functional autopair))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(web-mode-interpolate-color1-face ((t (:inherit web-mode-html-tag-face)))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
