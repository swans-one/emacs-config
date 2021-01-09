(require 'dash)

;; Utilities
;;;;;;;;;;;;

(defun first-matching (pattern list)
  "Return the first item in a list matching pattern, or nil if no match."
  (if (not list)
      nil
      (if (string-match pattern (car list))
          (car list)
        (first-matching pattern (cdr list)))))


;; One-Off Functions
;;;;;;;;;;;;;;;;;;;;

(defun erik-what-file ()
  (interactive)
  (message (buffer-file-name)))

(defun erik-unfill-paragraph (&optional region)
      "Takes a multi-line paragraph and makes it into a single line of text."
      (interactive (progn (barf-if-buffer-read-only) '(t)))
      (let ((fill-column (point-max))
            (emacs-lisp-docstring-fill-column t))
        (fill-paragraph nil region)))

(defun erik-unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (when (null (mark t)) (ding))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))))


(defun erik-collapse-line-back ()
  "Backwards collapse the line, repeat with backspace"
  (interactive)
  (back-to-indentation)
  (push-mark)
  (beginning-of-line)
  (backward-char 1)
  (delete-region (region-beginning) (region-end))
  (insert " ")
  ;; Only need to repeat backspace
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "<backspace>") 'erik-collapse-line-back)
     map)))

(defun erik-shell-echoes ()
  "If a shell is echoing input, this fixes it"
  (interactive)
  (setq comint-process-echoes t))


(defun erik-rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))


;; Org-mode functions
;;;;;;;;;;;;;;;;;;;;;

(defun erik-md-to-org-link ()
  (interactive)
  (search-backward "[" nil nil 1)
  (push-mark)
  (search-forward "]" nil nil 1)
  (kill-region 0 0 1)
  (delete-char 1)
  (insert "[[")
  (search-forward ")" nil nil 1)
  (delete-backward-char 1)
  (insert "]")
  (yank)
  (insert "]"))

(defun erik-org-publish-wiki (base-dir target-dir)
  "Publish the contents of a directory to a wiki format at the
  target location"
  (interactive "DBase Directory: \nDOutput Directory: ")
  (setq org-publish-project-alist
        `(("orgfiles"
           :base-directory ,base-dir
           :base-extension "org"
           :headline-levels 3
           :html-doctype "<!DOCTYPE html>"
           :html-head
           ,(string-join
             '("<link rel=\"stylesheet\""
               "      type=\"text/css\""
               "      href=\"/css/styles.css\""
               "/>"
               "")
             "\n")
           :html-head-include-default-style nil
           :html-head-include-scripts nil
           :html-html5-fancy t
           :html-inline-images t
           :html-preamble nil
           :html-postable nil
           :html-validation-link nil
           :publishing-directory ,target-dir
           :publishing-function org-html-publish-to-html
           :section-numbers nil
           :time-stamp-file nil
           :with-author nil
           :with-creator nil
           :with-date nil
           :with-email nil
           :with-timestamps nil
           :with-title nil
           :with-toc nil)
          ("static"
           :base-directory ,(concat base-dir "/static")
           :base-extension ".*"
           :publishing-directory ,(concat target-dir "/static")
           :publishing-function org-publish-attachment)
          ("js"
           :base-directory ,(concat base-dir "/js")
           :base-extension "js\\|jsx"
           :publishing-directory ,(concat target-dir "/js")
           :publishing-function org-publish-attachment)
          ("css"
           :base-directory ,(concat base-dir "/css")
           :base-extension "css"
           :publishing-directory ,(concat target-dir "/css")
           :publishing-function org-publish-attachment)
          ("wiki" :components ("orgfiles" "static" "js" "css"))))
  (message base-dir)
  (message target-dir)
  (message "%s" org-publish-project-alist)
  (org-publish "wiki")
)

;; Buffer Functions
;;;;;;;;;;;;;;;;;;;

(defun erik-reload-file ()
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

;; Terminal functions
;;;;;;;;;;;;;;;;;;;;;
(defun erik-term-send-to-shell (start end)
  (interactive "r")
  (process-send-region "*ansi-term*" start end))


(defun erik-term-run-last ()
  (interactive)
  (with-current-buffer "*ansi-term*" (term-send-up))
  (process-send-string"*ansi-term*" "\n"))

(defun erik-prev-term ()
  (interactive)
  (erik-rotate-term -1))

(defun erik-next-term ()
  (interactive)
  (erik-rotate-term 1))

(defun erik-rotate-term (dir)
  (let* ((buf (buffer-name))
         (term-num (erik-ansi-term-number buf)))
    (if (= term-num 0)
        (switch-to-buffer (get-buffer "*ansi-term*"))
      (switch-to-buffer
       (or (get-buffer (format "*ansi-term*<%d>" (+ term-num dir)))
           (get-buffer "*ansi-term*"))))))

(defun erik-ansi-term-number (buf-name)
  (let* ((regexp "\\*ansi-term\\*\\(<\\([0-9]+\\)>\\)?")
         (match (string-match regexp buf-name))
         (num (match-string 2 buf-name)))
    (cond ((not match) 0)
          ((not num) 1)
          ((string-to-number num)))))

(defun erik-toggle-ansi-term ()
  (interactive)
  (let ((open-ansi (get-window-with-predicate
                    (lambda (win)
                      (let* ((buf (window-buffer win))
                             (name (buffer-name buf)))
                        (string-match "\\*ansi-term\\*.*" name))))))
    (if open-ansi
        ;; If there is an window open with an ansi-term buffer, close it
        (delete-window open-ansi)
      ;; Otherwise create a new window & open the most recent ansi term in it
      (let ((new-window (split-window (frame-root-window) -10 'below))
            (recent-ansi-term (first-matching
                               "\\*ansi-term\\*.*"
                               (mapcar (lambda (x)
                                         (format "%s" x)) (buffer-list)))))
        (select-window new-window)
        (if recent-ansi-term ;; nil if none are open
            (set-window-buffer new-window recent-ansi-term)
          (ansi-term "/bin/bash"))))))


;; Underlining
;;;;;;;;;;;;;;
(defvar erik-underline-char-sequence '(?= ?- ?~ ?`))

(defun erik-underline-char (char)
  "Return the next appropriate character in the underline sequence"
  (let ((next-underline (member char erik-underline-char-sequence)))
    (if (and next-underline (nth 1 next-underline))
        (nth 1 next-underline)
      (car erik-underline-char-sequence))))

(defun erik-already-underlined (cur-under-char)
  "Take the first character and determine if it's an underline"
  (if (member cur-under-char erik-underline-char-sequence)
      t
    nil))

(defun erik-underline-line ()
  "Underline the current line, cycle through underlines"
  (interactive)
  (end-of-line)
  (let* ((cur-under-char (char-after (+ (point) 1)))
         (next-under-char (erik-underline-char cur-under-char))
         (column (current-column))
         (line-already-underlined (erik-already-underlined cur-under-char)))
    (if line-already-underlined
        (progn (forward-char)
            (delete-region (point) (line-end-position)))
      (insert-char ?\n))
    (insert-char next-under-char column))
  (beginning-of-line)
  (forward-line -1))

(defun erik-toggle-theme ()
  (interactive)
  (let ((is-light (-contains? custom-enabled-themes default-light-color-theme)))
    (dolist (theme custom-enabled-themes)
      (disable-theme theme))
    (load-theme
     (if is-light
         default-dark-color-theme
       default-light-color-theme))))


;; Keymap
(defvar erik-mode-map (make-sparse-keymap) "Keymap for erik-mode")

;; Full overrides
(define-key erik-mode-map (kbd "M-Q") 'erik-unfill-paragraph)
(define-key erik-mode-map (kbd "C-o") 'other-window)
(define-key erik-mode-map (kbd "C-c C-f") 'find-file-at-point)
(define-key erik-mode-map (kbd "<C-return>") 'electric-newline-and-maybe-indent)
(define-key erik-mode-map (kbd "C-x C-g") 'magit-status)
(define-key erik-mode-map (kbd "C-]") 'er/expand-region)
(define-key erik-mode-map (kbd "<C-M-backspace>") 'erik-collapse-line-back)

;; Erik-mode overrides
;;
;; C-j is the erik-mode prefix-key

;;
;; C-j Globals
;;
(define-key erik-mode-map (kbd "C-j w") 'erik-what-file)
(define-key erik-mode-map (kbd "C-j u") 'erik-underline-line)

;; C-j b _ -- Buffer manipulation
(define-key erik-mode-map (kbd "C-j b r") 'erik-reload-file)
(define-key erik-mode-map (kbd "C-j b n") 'erik-rename-file-and-buffer)

;; C-j t _ -- Terminal commands
(define-key erik-mode-map (kbd "C-j t s") 'erik-term-send-to-shell)
(define-key erik-mode-map (kbd "C-j t l") 'erik-term-run-last)
(define-key erik-mode-map (kbd "C-j t n") 'erik-next-term)
(define-key erik-mode-map (kbd "C-j t p") 'erik-prev-term)
(define-key erik-mode-map (kbd "C-j t t") 'erik-toggle-ansi-term)
(define-key erik-mode-map (kbd "C-j t o") 'ansi-term)

;; C-j o _ -- org mode stuff
(define-key erik-mode-map (kbd "C-j o l") 'erik-md-to-org-link)
(define-key erik-mode-map (kbd "C-j o p") 'erik-org-publish-wiki)

;; Binding functions I didn't write
(define-key erik-mode-map (kbd "C-j C-j") 'ido-select-text)
(define-key erik-mode-map (kbd "C-j e") 'eval-region)
(define-key erik-mode-map (kbd "C-j l") 'emmet-expand-line)


(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (when (not (eq (car (car minor-mode-map-alist)) 'erik-mode))
      (let ((mykeys (assq 'erik-mode minor-mode-map-alist)))
        (assq-delete-all 'erik-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

;;;###autoload
(define-minor-mode erik-mode
  "Tools that make sense given my emacs environment"
  :lighter " erik"
  :keymap erik-mode-map
  :global t
  )

;;;###autoload
(add-hook 'text-mode-hook 'erik-mode)

(provide 'erik-mode)
