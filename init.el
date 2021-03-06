;; init.el is my entry point for emacs customization. This file should
;; be used exclusively for setting up the load path, handling
;; packages, and requiring other files that contain the actual
;; customizations that I'm including.
;;
;; The customizations are further organized into the following files,
;; all within `site-lisp`:
;;
;;     - customize.el   = Simple Aesthetic customizations
;;     - defuns.el      = functions that I've written
;;     - keybindings.el = Global and mode specific keybindings
;;     - mode-mappings  = map filenames to major modes
;;     - hooks          = load elisp on major mode loading


;; Load Path / exec path
;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/modes" user-emacs-directory))

;; Append homebrew binary location to exec-path
(setq exec-path (append exec-path '("/usr/local/bin")))


;; Aesthetic customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'customize)


;; Package Management Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add other repositories to the package manager
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Package Configuration
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dash
  :ensure t)

(use-package elec-pair
  :demand t
  :config
  (electric-pair-mode)
  (defvar markdown-pairs '((?~ . ?~)))
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local electric-pair-pairs
                          (append electric-pair-pairs markdown-pairs)))))

(use-package emmet-mode
  :ensure t
  :hook (web-mode sgml-mode))

(use-package erik-mode
  :demand t
  :config
  (erik-mode))

(use-package ido ;; built-in
  :init (ido-mode t)
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  ;; don't backwards search for files
  (setq ido-auto-merge-work-directories-length -1))

(use-package magit
  :ensure t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package org ;; built-in
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-refile-targets '((nil :maxlevel . 6)
                             (org-agenda-files :maxlevel . 6)))
  (setq org-refile-use-outline-path 'file)
  (setq org-completion-use-ido t)
  (setq org-outline-path-complete-in-steps nil))

(use-package show-paren-mode ;; built-in
  :hook (emacs-lisp-mode scheme-mode))

(use-package term ;; built in
  :config
  (define-key term-raw-map (kbd "C-c C-y") 'term-paste))

(use-package tex-mode ;; built in
  :mode "\\.tex\\'"
  :config
  (setq latex-run-command "pdflatex")
  (setenv "PATH" (concat
                  "/usr/texbin" ":"
                  "/usr/local/bin" ":"
                  "/usr/local/sbin" ":"
                  (getenv "PATH")))
  )

(use-package uniquify-files
  :ensure t
  :config
  (setq uniquify-buffer-name-style 'post-forward))

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.js[x]?\\'" . web-mode)
         ("\\.svelte\\'" . web-mode))
  :config
  (setq web-mode-enable-auto-closing t
        web-mode-enable-auto-pairing t
        web-mode-auto-close-style 1
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-script-padding 2
        web-mode-style-padding 2
        web-mode-block-padding 2)
  )

(use-package yasnippet
  :ensure t
  :init
  ;; disable for term-mode
  (add-hook 'term-mode-hook (lambda () (yas-minor-mode -1)))
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (setq yas-prompt-functions '(yas-ido-prompt))
  (yas-global-mode 1))


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
