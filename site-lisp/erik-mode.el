(require 'dash)

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
  (setq erik-unfill-count (+ erik-unfill-count 1))
  (when (called-interactively-p 'interactive)
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "<backspace>") 'erik-collapse-line-back)
       map)))
  (back-to-indentation)
  (push-mark)
  (beginning-of-line)
  (backward-char 1)
  (delete-region (region-beginning) (region-end))
  (insert " "))

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

;; Keymap
(defvar erik-mode-map (make-sparse-keymap) "Keymap for erik-mode")

;; Full overrides
(define-key erik-mode-map (kbd "M-Q") 'erik-unfill-paragraph)
;;(define-key erik-mode-map (kbd "C-M <backspace>") 'erik-collapse-line-back)

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

;; C-j t _ -- Terminal commands
(define-key erik-mode-map (kbd "C-j t s") 'erik-term-send-to-shell)
(define-key erik-mode-map (kbd "C-j t l") 'erik-term-run-last)
(define-key erik-mode-map (kbd "C-j t n") 'erik-next-term)
(define-key erik-mode-map (kbd "C-j t p") 'erik-prev-term)
(define-key erik-mode-map (kbd "C-j t t") 'erik-toggle-ansi-term)
(define-key erik-mode-map (kbd "C-j t o") 'ansi-term)

;; C-j o _ -- org mode stuff
(define-key erik-mode-map (kbd "C-j o l") 'erik-md-to-org-link)

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
