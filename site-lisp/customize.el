;; Get rid of visual cruft
(when (display-graphic-p)
  (tool-bar-mode -1)
  (menu-bar-mode -1))
(scroll-bar-mode -1)
(setq column-number-mode t)
(setq inhibit-startup-message t)

;; Editing improvements: transient-mark, tabs, subword mode
(transient-mark-mode t)
(setq-default indent-tabs-mode nil)
(global-subword-mode 1)

;; Ido Mode
;;;;;;;;;;;
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)
(setq ido-auto-merge-work-directories-length -1) ;; don't backwards search for files
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; Make the text size reasonable
(set-face-attribute 'default nil :height 100)

;; Visible Bell instead of Audible bell
(defun my-terminal-visible-bell ()
   "A friendlier visual bell effect."
   (invert-face 'mode-line)
   (run-with-timer 0.1 nil 'invert-face 'mode-line))
(setq visible-bell nil ring-bell-function 'my-terminal-visible-bell)

;; Save all autosave files in a backups directory
(defvar user-temporary-file-directory
  "~/.emacs-backup")
(setq backup-directory-alist
      `((".*" . ,user-temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

;; Use pdflatex to compile LaTeX files
(setq latex-run-command "pdflatex")
(setenv "PATH" (concat
                "/usr/texbin" ":"
                "/usr/local/bin" ":"
                "/usr/local/sbin" ":"
                (getenv "PATH")))

;; Custom options for `C-u M-x ps-print-buffer-with-faces`
(setq ps-print-header nil)

;; Load Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'ample-zen t)

;; Set exec-path to find homebrew binaries
(setq exec-path (append exec-path '("/usr/local/bin")))

;; enable auto-pair
(require 'autopair)
(autopair-global-mode)


;; Customize Options (defcustom / defvar)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom default-dark-color-theme 'ample-zen
  "Default dark color-scheme")
(defcustom default-light-color-theme 'whiteboard
  "Default light color-scheme")

(defvar mode-cycles
  (list '(web-mode js2-mode)))


;; Erik-mode
;;;;;;;;;;;;
(require 'erik-mode)

;; C-mode
;;;;;;;;;

(setq-default c-default-style "linux"
              c-basic-offset 4)


;; YaSnippet
;;;;;;;;;;;;

(require 'yasnippet)
(with-eval-after-load 'yasnippet
    (setq yas-snippet-dirs '("~/.emacs.d/snippets")))
(yas-global-mode 1)
(setq yas-prompt-functions '(yas-ido-prompt))


;; Web Mode / JSX mode / purescript mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use for html
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; auto close html elements on >
(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-auto-pairing t)
(setq web-mode-auto-close-style 1)

(add-hook 'purescript-mode-hook
  (lambda ()
    (turn-on-purescript-indentation)))
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)


;; Misc Mode Requires
;;;;;;;;;;;;;;;;;;;;;

(require 'dcpu16-mode)
(require 'puppet-mode)
(require 'markdown-mode)
(require 'rainbow-delimiters)

;; If this doesn't work, need to build/install ESS
;     cd && mkdir .ESS && cd .ESS
;     git clone git://github.com/emacs-ess/ESS.git && cd ESS && make all
; (load "~/.ESS/ESS/lisp/ess-site")


(provide 'customize)
