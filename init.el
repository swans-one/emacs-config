;;; ...  -*- lexical-binding: t -*-

;; init.el is my entry point for emacs customization.
;;
;; The customizations are further organized into the following files,
;; all within `lisp`:
;;
;;     - customize.el   = Simple Aesthetic customizations
;;     - keybinds.el    = Global keybindings (and unbindings)
;;
;; These files should be loaded before the rest of the configuration


;; Load Path / exec path
;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/modes" user-emacs-directory))

;; Append homebrew and local binary locations to exec-path
(setq exec-path (append exec-path '("/usr/local/bin" "~/.local/bin")))

;; Don't pop up the warnings buffer for native compilation
(setq native-comp-async-report-warnings-errors 'silent)

;; Aesthetic customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'customize)

;; Global Keybinds
;;;;;;;;;;;;;;;;;;

;; Note, this includes unbindings which may be useful later
;; TODO: move some global bindings from erik-mode here
(require 'keybinds)

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

(use-package ace-window
  :ensure t
  :bind ("M-o" . 'ace-window)
  :config  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l))
  ;; Make sure 'ace-window is bound for ansi-term as well
  :hook
  (term-mode . (lambda () (define-key term-raw-map (kbd "M-o") 'ace-window))))

(use-package caddyfile-mode
  :ensure t
  :mode (("Caddyfile\\'" . caddyfile-mode)
         ("caddy\\.conf\\'" . caddyfile-mode))
  :hook
  ;; This has to be a hook because indent-tabs-mode and tab-width are
  ;; buffer-local variables, so we can't modify them in `config:` as
  ;; that is only run once when caddyfile-mode is loaded.
  (caddyfile-mode . (lambda ()
                      (setq indent-tabs-mode t)
                      (setq tab-width 4))))


(use-package cnfonts
  :ensure t
  :config
  (cnfonts-mode 1))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode))

(use-package css-mode
  :ensure t
  :config
  (setq-default css-indent-offset 2))

(use-package dash
  :ensure t)

(use-package dash-functional
  :ensure t)

(use-package eglot
  :ensure t
  :hook
  ;; Disable flymake by default. Can enable with ~M-x flymake-mode~
  (eglot-managed-mode . (lambda ()
                          (flymake-mode -1)
                          (eglot-inlay-hints-mode -1)))
  :config
  ;; When using xref, still have eglot active in the followed location
  (setq eglot-extend-to-xref t)
  ;; Activate the node environment where the language server is installed
  ;; TODO: have this be project specific / configurable
  (nvm-use "v18.15.0")
  (add-to-list 'eglot-server-programs '(web-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(web-mode . ("svelteserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
                 ("rust-analyzer" :initializationOptions (:check (:command "clippy"))))))

(use-package electric
  :demand t
  :hook
  (yaml-mode . electric-indent-local-mode)
  (rust-mode . electric-indent-local-mode)
  (web-mode . electric-indent-local-mode)
  (python-mode . electric-indent-local-mode)
  :config
  (electric-indent-mode -1))

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

(use-package json-mode
  :ensure t)

(use-package magit
  :ensure t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package nvm
  :ensure t)

(defun yas-org-very-safe-expand ()
  (let ((yas-fallback-behavior 'return-nil)) (yas-expand)))

(use-package org ;; built-in
  :mode ("\\.org\\'" . org-mode)
  :bind
  (:map org-mode-map ("C-c l" . 'org-store-link))
  :hook
  ;; Fix org mode issue? https://orgmode.org/manual/Conflicts.html#index-yasnippet_002eel
  (org-mode . (lambda ()
                (make-variable-buffer-local 'yas-trigger-key)
                (setq yas-trigger-key [tab])
                (add-to-list 'org-tab-first-hook 'yas-org-very-safe-expand)
                (define-key yas-keymap [tab] 'yas-next-field)))

  :config
  (setq org-refile-targets '((nil :maxlevel . 6)
                             (org-agenda-files :maxlevel . 6)))
  (setq org-refile-use-outline-path 'file)
  (setq org-completion-use-ido t)
  (setq org-outline-path-complete-in-steps nil)

  ;; When calling org-do-demote / org-do-promote via M-right / M-left
  ;; on a header don't also indent the body.
  ;;
  ;; See: https://emacs.stackexchange.com/questions/41220/org-mode-disable-indentation-when-promoting-and-demoting-trees-subtrees
  (setq org-adapt-indentation nil)
)

(use-package org-slug
  :load-path "lisp"
  :after org)

;; used by the `pyim` package for popups
(use-package posframe
  :ensure t)

(use-package pyvenv
  :ensure t)

;; pyim chinese input method. Toggle to chinese with `C-\ pyim`
;; The package `posframe` is used for inline popups
;; See the `cnfonts` package for chinese font-size config
(use-package pyim
  :ensure t
  :after (posframe pyim-basedict)
  :custom
  (pyim-page-tooltip 'posframe)
  (pyim-page-length 5)
  (pyim-page-posframe-border-width 2)
  (pyim-page-posframe-min-width 30)
  (pyim-page-style 'vertical) ;; One of: one-line, two-lines, vertical, minibuffer
  :config
  (pyim-basedict-enable))

(use-package pyim-basedict
  :ensure t)

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")

(use-package show-paren-mode ;; built-in
  :hook (emacs-lisp-mode scheme-mode))

(use-package sql ;; built in
  :config
  ;; Fix non-idempotent indent in sql-mode breaking yasnippet mirrors
  ;; See: https://emacs.stackexchange.com/a/31917/12244
  (add-hook 'sql-mode-hook
            (lambda () (setq-local yas-indent-line 'fixed))))

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
                  (getenv "PATH"))))

(use-package uniquify-files
  :ensure t
  :config
  (setq uniquify-buffer-name-style 'post-forward))

(defun my-web-mode-end-fix (mvmt-fn)
  "Fix behaviour of movement from the end of an element
If point is at the end of an element
(e.g. after calling web-mode-element-end) Then most movement
functions should behave as if you're still inside that
element. Unless you land at the begining of the next element."
  (lambda ()
    (interactive)
    (if (and (eq ?> (char-before)) (not (eq ?< (char-after))))
        (progn
          (backward-char)
          (call-interactively mvmt-fn))
      (call-interactively mvmt-fn))))

(defalias 'my-web-mode-element-beginning-2
  (my-web-mode-end-fix 'web-mode-element-beginning))
(defalias 'my-web-mode-element-sibling-previous-2
  (my-web-mode-end-fix 'web-mode-element-sibling-previous))

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.js[x]?\\'" . web-mode)
         ("\\.svelte\\'" . web-mode)
         ("\\.ts\\'" . web-mode))
  :bind
  (:map web-mode-map
        ("C-c C-n" . web-mode-element-sibling-next)
        ("C-c C-p" . web-mode-element-sibling-previous)
        ("C-c C-f" . web-mode-element-parent)
        ("C-c C-b" . web-mode-element-child))
  (:repeat-map web-mode-repeat-map
               ("n" . web-mode-element-sibling-next)
               ("p" . my-web-mode-element-sibling-previous-2)
               ("f" . web-mode-element-child)
               ("b" . web-mode-element-parent)
               ("e" . web-mode-element-end)
               ("a" . my-web-mode-element-beginning-2))
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
   '("9494d6d64290602054292f7c1b2db4285b3fea4fbf63b54bdac21aa6f6b0a7e6" "f897f31a459baa86363c91ab0d98d184e41d42fd2c33ec39e72561f25bd8138b" default))
 '(package-selected-packages
   '(pyim-basedict pyim caddyfile-mode corfu json-mode ace-window nvm eglot neotree pyvenv flycheck rust-mode htmlize use-package gnu-elpa-keyring-update yaml-mode emmet-mode web-mode yasnippet quack paredit magit haskell-mode geiser expand-region dash-functional autopair)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(web-mode-interpolate-color1-face ((t (:inherit web-mode-html-tag-face)))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
