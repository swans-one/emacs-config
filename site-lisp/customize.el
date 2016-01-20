;; Get rid of visual cruft
(if (display-graphic-p)
    (tool-bar-mode 0)
  (menu-bar-mode 0))
(scroll-bar-mode -1)
(setq column-number-mode t)
(setq inhibit-startup-message t)

;; Editing improvements: transient-mark, tabs, subword mode
(transient-mark-mode t)
(setq-default indent-tabs-mode nil)
(global-subword-mode 1)

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


;; Ido Mode
;;;;;;;;;;;

(require 'ido)
(ido-mode t)
(setq ido-auto-merge-work-directories-length -1) ;; don't backwards search for files
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)


;; C-mode
;;;;;;;;;

(setq-default c-default-style "linux"
              c-basic-offset 4)


;; Org Mode Customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file "~/captured-notes.org")


;; YaSnippet
;;;;;;;;;;;;

(require 'yasnippet)
(with-eval-after-load 'yasnippet
    (setq yas-snippet-dirs '("~/.emacs.d/snippets")))
(yas-global-mode 1)
(setq yas-prompt-functions '(yas-ido-prompt))


;; Web Mode / JSX mode
;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))

(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")))

; auto close html elements on >
(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-auto-pairing t)
(setq web-mode-auto-close-style 2)


;; Misc Mode Requires
;;;;;;;;;;;;;;;;;;;;;

(require 'dcpu16-mode)
(require 'puppet-mode)
(require 'markdown-mode)
(require 'rainbow-delimiters)

;; If this doesn't work, need to build/install ESS
;     cd && mkdir .ESS && cd .ESS
;     git clone git://github.com/emacs-ess/ESS.git && cd ESS && make all
(load "~/.ESS/ESS/lisp/ess-site")


(provide 'customize)
