;;
;; Individual functions I've written
;;


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
    (search-backward "class")
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


(provide 'defuns)
