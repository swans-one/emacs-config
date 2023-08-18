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

;; Append homebrew and local binary locations to exec-path
(setq exec-path (append exec-path '("/usr/local/bin" "~/.local/bin")))


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

(use-package dash-functional
  :ensure t)

(use-package eglot
  :ensure t
  :hook
  ;; Disable flymake by default. Can enable with ~M-x flymake-mode~
  (eglot-managed-mode . (lambda () (flymake-mode -1)))
  :config
  (setq eglot-extend-to-xref t))

(use-package elec-pair
  :demand t
  :config
  (electric-pair-mode)
  (defvar markdown-pairs '((?~ . ?~)))
  (defvar web-mode-pairs '((?' . ?')))
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local electric-pair-pairs
                          (append electric-pair-pairs markdown-pairs))))
  (add-hook 'web-mode-hook
            (lambda ()
              (setq-local electric-pair-pairs
                          (append electric-pair-pairs web-mode-pairs)))))

(use-package emmet-mode
  :ensure t
  :hook (web-mode sgml-mode))

(use-package erik-mode
  :demand t
  :config
  (erik-mode))

(use-package flycheck
  :ensure t
  :config
  (setq flycheck-check-syntax-automatically '(save))
  (setq flycheck-disabled-checkers '(rust-cargo)))

(use-package flymake)

(use-package htmlize
  :ensure t
  :defer t)

(use-package ido ;; built-in
  :init
  (ido-mode t)
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  ;; don't backwards search for files
  (setq ido-auto-merge-work-directories-length -1))

(use-package lsp-mode
  :ensure t
  :hook (rust-mode . lsp)
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-diagnostics-provider :flycheck)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-modeline-workspace-status-enable nil)
  (define-key lsp-mode-map (kbd "M-l") lsp-command-map)
  :commands lsp)

(use-package lsp-ui
  :ensure t)

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
  :bind
  (:map org-mode-map ("C-c l" . 'org-store-link))
  :config
  (setq org-refile-targets '((nil :maxlevel . 6)
                             (org-agenda-files :maxlevel . 6)))
  (setq org-refile-use-outline-path 'file)
  (setq org-completion-use-ido t)
  (setq org-outline-path-complete-in-steps nil))

(use-package pyvenv
  :ensure t)

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :config
  (add-hook 'rust-mode-hook #'electric-indent-local-mode))

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

(use-package yaml-mode
  :ensure t)

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
    (eglot neotree pyvenv lsp-ui flycheck lsp-mode rust-mode htmlize use-package gnu-elpa-keyring-update yaml-mode emmet-mode web-mode yasnippet quack paredit magit haskell-mode geiser expand-region dash-functional autopair))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(web-mode-interpolate-color1-face ((t (:inherit web-mode-html-tag-face)))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
