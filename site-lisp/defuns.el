;;
;; Individual functions I've written
;;
(require 'dash)


;; LaTeX keybindings
(defun insert-dollar-pair ()
  "Insert a pair of dollar signs at point, then move point
between them."
  (interactive)
  (insert "$$")
  (backward-char 1)
)
(defun enter-display-math ()
  "Insert the LaTeX display-math environment amd move the point
inside of it."
  (interactive)
  (if (not (equal (current-column) 0))
      (insert "\n\n"))
  (insert "    \\[\n")
  (insert "    \n")
  (insert "    \\]\n")
  (backward-char 8)
)

;; What file am I looking at.
(defun what-file () (interactive) (message (buffer-file-name)))

;; Django Template function
(defun insert-new-django-block ()
  "Insert a new block statement, move point to block name."
  (interactive)
  (insert "{% block %}{% endblock %}")
  (backward-char 16)
)

;; Python what-class
(defun python-what-class ()
  (interactive)
  (save-excursion
    (search-backward "\nclass")
    (forward-char)
    (message (replace-regexp-in-string
              "\n$" ""
              (thing-at-point 'line)))
))


;; Wrap a single line.
(defun wrap-line ()
  "Wrap a line to 72 characters or less"
  (interactive)
  (move-to-column 72)
  (if (and (not (equal (char-after) " "))
           (not (equal (char-before) " ")))
      (backward-word))
  (backward-word)
  (forward-word)
  (insert "\n")
  (funcall indent-line-function)
  (end-of-line)
)
(defun wrap-line-indent ()
  "Wrap a line to 72 characters than indent it by four."
  (interactive)
  (wrap-line)
  (back-to-indentation)
  (insert "    ")
  (end-of-line)
)

;; Run-commented
(defun run-commented ()
  "Uncommnet, run, recomment a paragraph"
  (interactive)
  (mark-paragraph)
  (exchange-point-and-mark)
  (backward-char)
  (uncomment-region (region-beginning) (region-end))
  (call-interactively (key-binding (kbd "C-M-x")))
  (comment-region (region-beginning) (region-end))
)

;; Underlining
(setq underline-char-sequence '(?= ?- ?~ ?`))

(defun underline-char (char)
  "Return the next appropriate character in the underline sequence"
  (let ((next-underline (member char underline-char-sequence)))
    (if (and next-underline (nth 1 next-underline))
        (nth 1 next-underline)
      (car underline-char-sequence))))

(defun already-underlined (cur-under-char)
  "Take the first character and determine if it's an underline"
  (if (member cur-under-char underline-char-sequence)
      t
    nil))

(defun underline-line ()
  ""
  (interactive)
  (end-of-line)
  (let* ((cur-under-char (char-after (+ (point) 1)))
         (next-under-char (underline-char cur-under-char))
         (column (current-column))
         (line-already-underlined (already-underlined cur-under-char)))
    (if line-already-underlined
        (progn (forward-char)
            (delete-region (point) (line-end-position)))
      (insert-char ?\n))
    (insert-char next-under-char column))
  (beginning-of-line)
  (forward-line -1))

;; Pbcopy function
(defun pbcopy ()
  "Copy to the osx clipboard"
  (interactive)
  (shell-command-on-region
   (region-beginning) (region-end) "pbcopy"))


;; Unfill paragraph
(defun unfill-paragraph (&optional region)
      "Takes a multi-line paragraph and makes it into a single line of text."
      (interactive (progn (barf-if-buffer-read-only) '(t)))
      (let ((fill-column (point-max))
            (emacs-lisp-docstring-fill-column t))
        (fill-paragraph nil region)))

;; Compile with ./build.sh
(defun compile-build-sh ()
  (interactive)
  (compile "./build.sh"))


;; Make an echoing shell behave correctly
(defun shell-echoes ()
  (interactive)
  (setq comint-process-echoes t))

;; Uncycle through mark ring (opposite of C-u C-spc
(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (when (null (mark t)) (ding))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))))


(defun collapse-line-back ()
  (interactive)
  (back-to-indentation)
  (push-mark)
  (beginning-of-line)
  (backward-char 1)
  (delete-region (region-beginning) (region-end))
  (insert " "))

;; Toggle between light and dark themes
(defun toggle-theme ()
  (interactive)
  (let ((is-light (find default-light-color-theme custom-enabled-themes)))
    (dolist (theme custom-enabled-themes)
      (disable-theme theme))
    (load-theme
     (if is-light
         default-dark-color-theme
       default-light-color-theme))))

;; Read from defined mode cycles, and cycle through them.
;;   - For example, switch between web-mode, js2-mode, and js3-mode
(defun last-item (l)
  (if (eq (cdr l) '())
      (car l)
    (last-item (cdr l))))
(defun next-in-cycle (item cycle)
  (cond ((not (-contains? cycle item)) (error "item <%s> not in cycle" item))
        ((eq item (last-item cycle)) (car cycle))
        (t (cadr (member item cycle))))
  )
(defun rotate-mode ()
  (interactive)
  (let ((current-mode-list
         (car (-filter (lambda (l) (-contains? l major-mode)) mode-cycles))))
    (if (eq current-mode-list '())
        (message "mode <%s> not included in a mode list" major-mode)
      (funcall (next-in-cycle major-mode current-mode-list)))))


(provide 'defuns)
