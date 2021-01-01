;;
;; Individual functions I've written
;;
(require 'dash)


;; Python what-class
(defun my/python-what-class ()
  (interactive)
  (save-excursion
    (search-backward "\nclass")
    (forward-char)
    (message (replace-regexp-in-string
              "\n$" ""
              (thing-at-point 'line)))
))


;; Wrap a single line.
(defun my/wrap-line ()
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
(defun my/wrap-line-indent ()
  "Wrap a line to 72 characters than indent it by four."
  (interactive)
  (my/wrap-line)
  (back-to-indentation)
  (insert "    ")
  (end-of-line)
)

;; Run-commented
(defun my/run-commented ()
  "Uncommnet, run, recomment a paragraph"
  (interactive)
  (mark-paragraph)
  (exchange-point-and-mark)
  (backward-char)
  (uncomment-region (region-beginning) (region-end))
  (call-interactively (key-binding (kbd "C-M-x")))
  (comment-region (region-beginning) (region-end))
)

;; Pbcopy function
(defun my/pbcopy ()
  "Copy to the osx clipboard"
  (interactive)
  (shell-command-on-region
   (region-beginning) (region-end) "pbcopy"))


;; Unfill paragraph
(defun my/unfill-paragraph (&optional region)
      "Takes a multi-line paragraph and makes it into a single line of text."
      (interactive (progn (barf-if-buffer-read-only) '(t)))
      (let ((fill-column (point-max))
            (emacs-lisp-docstring-fill-column t))
        (fill-paragraph nil region)))

;; Compile with ./build.sh
(defun my/compile-build-sh ()
  (interactive)
  (compile "./build.sh"))


;; Make an echoing shell behave correctly
(defun my/shell-echoes ()
  (interactive)
  (setq comint-process-echoes t))

;; Uncycle through mark ring (opposite of C-u C-spc
(defun my/unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (when (null (mark t)) (ding))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))))


;; Toggle between light and dark themes
(defun my/toggle-theme ()
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
(defun my/last-item (l)
  (if (eq (cdr l) '())
      (car l)
    (my/last-item (cdr l))))
(defun my/next-in-cycle (item cycle)
  (cond ((not (-contains? cycle item)) (error "item <%s> not in cycle" item))
        ((eq item (my/last-item cycle)) (car cycle))
        (t (cadr (member item cycle))))
  )
(defun my/rotate-mode ()
  (interactive)
  (let ((current-mode-list
         (car (-filter (lambda (l) (-contains? l major-mode)) mode-cycles))))
    (if (eq current-mode-list '())
        (message "mode <%s> not included in a mode list" major-mode)
      (funcall (my/next-in-cycle major-mode current-mode-list)))))


;; Rename the file for a given buffer
(defun my/rename-file-and-buffer (new-name)
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

(provide 'defuns)
