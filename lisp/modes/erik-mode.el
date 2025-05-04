;; Utilities
;;;;;;;;;;;;

(defun first-matching (pattern list)
  "Return the first item in a LIST matching PATTERN, or nil if no match."
  (if (not list)
      nil
      (if (string-match pattern (car list))
          (car list)
        (first-matching pattern (cdr list)))))

(defun parent (loc) (file-name-directory (directory-file-name loc)))

(defun collect-regex-matches (regex &optional capture-group start end)
  "Return all matches of a regex in the current buffer.
Optionally:
  non-nil `capture-group' will return the value of the group.
  non-nil `start' will start the search at the given position.
  non-nil `end' will end the search at the given position."
  (let (
        (begin (or start (point-min)))
        (stop (or end (point-max)))
        (matches nil))
    (goto-char begin)
    (while (re-search-forward regex stop t)
      (push (match-string-no-properties capture-group) matches)
    )
    matches))

;; Config-file utilties
;;   - Find a project-level config from the current file
;;   - Read and parse that file
;;   - Use the values, or prompt
(defun find-project-config (config-name &optional current-location)
  "Find a config file upwards in the directory tree.
CONFIG-NAME is a file name to search upwards in the directory
tree for.  CURRENT-LOCATION is a directory to search upward from,
it defaults to the file of the current buffer"
  (cond ((null (or current-location (buffer-file-name))) nil)
        ((null current-location)
         (find-project-config config-name (buffer-file-name)))
        (t (let* ((up (parent current-location))
                  (config-file (concat up config-name)))
             (cond ((null current-location) nil)
                   ((file-readable-p config-file) config-file)
                   ((string= up "/") nil)
                   (t (find-project-config config-name up)))))))

(defun config-blank-p (s) (string= (string-trim-right s) ""))
(defun config-comment-p (s) (string-prefix-p "#" s))
(defun parse-config-lines (lines alist)
  "Parse a list of strings in key=value format into an alist"
  (let* ((first (car lines))
         (pair (split-string first "="))
         (key (car pair))
         (value (cadr pair)))
    (if (null (cdr lines))
        (cons (cons key value) alist)
      (parse-config-lines (cdr lines) (cons (cons key value) alist)))))

(defun read-config-file (filename)
  "Read in a ini style config file into an alist FILENAME should
point to a file with each line containing key-value pairs in the
ini format \"key=val\". Empty lines and lines starting with #
will be ignored. Values starting with ./ have the ./ replaced by
the directory of the config file."
  (let* ((directory-name (parent filename))
         (file-contents (with-temp-buffer
                          (insert-file-contents filename)
                          (buffer-string)))
         (file-lines (split-string file-contents "\n"))
         (config-lines (-reject
                        (-orfn 'config-comment-p 'config-blank-p)
                        file-lines)))
    (-map (lambda (pair)
            (-let (((key . val) pair))
              (cons key (replace-regexp-in-string "^\./" directory-name val))))
          (parse-config-lines config-lines '()))
    ))

(defun config-or-prompt (config-alist prompt-alist &optional out-alist)
  "For each prompt, first check if the key is in config-alist,
  otherwise prompt the user for a value. Returns a alist."
  (let* ((key (caar prompt-alist))
         (fn (cdar prompt-alist))
         (prompt-rest (cdr prompt-alist))
         (config-val (cdr (assoc key config-alist)))
         (val (if config-val config-val (funcall fn)))
         (out-alist (cons (cons key val) out-alist)))
    (if (null prompt-rest)
        out-alist
      (config-or-prompt config-alist prompt-rest out-alist))))


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

;; Lisp eval function
;;;;;;;;;;;;;;;;;;;;;

(defun erik-eval-and-print (start end)
  "Run eval-region but print to the current buffer at point"
  (interactive "r")
  (eval-region start end (current-buffer)))

(defun erik-eval-and-print-to-output (start end)
  "Run eval-region but print to the buffer *Lisp Output*"
  (interactive "r")
  (let ((message-buffer (get-buffer-create "*Lisp Output*")))
    (with-current-buffer message-buffer
      (goto-char (point-max))
      (insert-char ?\n))
    (eval-region start end message-buffer)))


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

(defun erik-org-publish-wiki ()
  "Publish the contents of a directory to a wiki format at the
  target location"
  (interactive)
  ;; Dynamic binding allows us to override this definition. We do
  ;; mostly the same thing, but we introduce an additional wrapper div
  ;; around the main content. This will make reflowing with grid
  ;; easier.
  ;;
  ;; TODO: Instead of binding over this function, create a derived
  ;; backend of the html backend, which just replaces this function.
  (defun org-html-inner-template (contents info)
    (concat
     ;; Table of contents.
     (let ((depth (plist-get info :with-toc)))
       (when depth (org-html-toc depth info)))
     ;; Document contents.
     "<div id=contents-main>"
     contents
     "</div>"
     ;; Footnotes section.
     (org-html-footnote-section info)))

  (let* ((config-file-name (find-project-config "publish.conf"))
         (config-file-alist
          (if config-file-name (read-config-file config-file-name) '()))
         (prompts
          '(("src-dir" . (lambda () (read-directory-name "Src dir: ")))
            ("target-dir" . (lambda () (read-directory-name "Target dir: ")))))
         (config (config-or-prompt config-file-alist prompts)))

    (message "%s" config)
    (message (cdr (assoc "src-dir" config))) ;; Todo: fix path implementation
    (message (cdr (assoc "target-dir" config)))
    (cl-flet ((config-val (lambda (key) (cdr (assoc key config)))))
      (setq org-publish-project-alist
            `(("orgfiles"
               :base-directory ,(config-val "src-dir")
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
               :html-indent nil ;; indenting will mess with <pre> tags
               :html-inline-images t
               :html-preamble
               ,(string-join
                 '("<nav>"
                   "<a href=\"/\">Home</a>"
                   "</nav>"
                   )
                 "\n")
               :html-postable nil
               :html-validation-link nil
               :publishing-directory ,(config-val "target-dir")
               :publishing-function org-html-publish-to-html
               :section-numbers nil
               :time-stamp-file nil
               :with-author nil
               :with-creator nil
               :with-date nil
               :with-email nil
               :with-timestamps nil
               :with-title t
                                        ; :with-toc nil
               )
              ("static"
               :base-directory
               ,(concat (config-val "base-dir") "/static")
               :base-extension ".*"
               :publishing-directory
               ,(concat (config-val "target-dir") "/static")
               :publishing-function org-publish-attachment)
              ("js"
               :base-directory ,(concat (config-val "base-dir") "/js")
               :base-extension "js\\|jsx"
               :publishing-directory ,(concat (config-val "target-dir") "/js")
               :publishing-function org-publish-attachment)
              ("css"
               :base-directory ,(concat (config-val "base-dir") "/css")
               :base-extension "css"
               :publishing-directory ,(concat (config-val "target-dir") "/css")
               :publishing-function org-publish-attachment)
              ("wiki" :components ("orgfiles" "static" "js" "css")))))
    (org-publish "wiki")))

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

(defun erik-ansi-window ()
  "Return the window with an ansi-term buffer in it, or nil if not open"
  (get-window-with-predicate
   (lambda (win) (string-match "\\*ansi-term\\*.*"
                               (buffer-name (window-buffer win))))))

(defun erik-get-or-create-ansi-buffer ()
  "Return a buffer with an ansi-term window creating it if it doesn't exist"
  (or (car (match-buffers
            (lambda (buf) (string-match "\\*ansi-term\\*.*" (buffer-name buf)))))
      (save-window-excursion (ansi-term "/bin/bash"))))

(defun erik-toggle-ansi-term ()
  "Open / close an ansi-term tray.

If there is an existing ansi-term buffer, it will be opened,
otherwise one will be created"
  (interactive)
  (let ((open-ansi (erik-ansi-window)))
    (if open-ansi
        ;; If there is an window open with an ansi-term buffer, close it
        (delete-window open-ansi)
      ;; Otherwise create a new window & open the most recent ansi term in it
      (let* ((ansi-buf (erik-get-or-create-ansi-buffer))
             (bottom-win (display-buffer-in-side-window
                          ansi-buf
                          '((side . bottom) (slot . 0)))))
        (window-resize bottom-win -4)
        (select-window bottom-win)))))


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

(defun erik-flymake-eglot-toggle ()
  "Toggle flymake in an eglot managed buffer"
  (interactive)
  (if (bound-and-true-p flymake-mode)
      (eglot-reconnect (eglot--current-server-or-lose) t)
    (flymake-mode)))


;; Web-mode paredit-isms
;;;;;;;;;;;;;;;;;;;;;;;;
(defun erik-web-mode-slurp-text ()
  "Slurp in the sibling text"
  (interactive)
  (save-excursion
    (web-mode-element-end)
    (set-mark (point))
    (web-mode-tag-next)
    (kill-region (mark) (point))
    (web-mode-element-previous)
    (web-mode-tag-next)
    (yank)))

(defun erik-web-mode-slurp-node ()
  "Slurp in the sibling node"
  (interactive)
  (save-excursion
    (web-mode-element-next)
    (web-mode-element-select)
    (kill-region (mark) (point))
    (web-mode-tag-previous)
    (yank)))


;; Chinese input method functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun erik-input-pinyin ()
  "Set input method to chinese-sisheng"
  (interactive)
  (set-input-method "chinese-sisheng"))

(defun erik-input-deactivate ()
  "Deactivate the current input method"
  (interactive)
  (deactivate-input-method))

(defun erik-input-chinese ()
  "Set input method to pyim"
  (interactive)
  (set-input-method "pyim"))

(defun erik-chinese-lookup-character ()
  (interactive)
  (let* ((char (char-to-string (char-after)))
        (url (format "https://dong-chinese.com/wiki/%s" char)))
    (browse-url url)))

(defun erik-chinese-lookup-word (start end)
  (interactive "r")
  (let* ((word (buffer-substring start end))
         (url (format "https://dong-chinese.com/wiki/%s" word)))
    (browse-url url)))

;; Anki authoring functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun erik-anki-make-cloze-buffer (start end)
  "Take a region and turn it into a close, numbered based on the
current buffer"
  (interactive "r")
  (let* ((cloze-regex "cl\\([[:digit:]]+\\)::")
         (matches (collect-regex-matches cloze-regex 1))
         (highest (car (sort (mapcar 'string-to-number matches) #'>))))
    (goto-char end)
    (insert "}}")
    (save-excursion
      (goto-char start)
      (insert (format "{{cl%s::" (+ highest 1))))))

(defun erik-anki-make-cloze-line (start end)
  "Take a region and turn it into a close, numbered based on the
current line"
  (interactive "r")
  (let* ((cloze-regex "cl\\([[:digit:]]+\\)::")
         (search-start (line-beginning-position))
         (search-end (line-end-position))
         (matches (collect-regex-matches cloze-regex 1 search-start search-end))
         (highest (or
                   (car (sort (mapcar 'string-to-number matches) #'>))
                   0)))
    (goto-char end)
    (insert "}}")
    (save-excursion
      (goto-char start)
      (insert (format "{{cl%s::" (+ highest 1))))))


;; Keymap
(defvar erik-mode-map (make-sparse-keymap) "Keymap for erik-mode")

;; Full overrides
(define-key erik-mode-map (kbd "M-Q") 'erik-unfill-paragraph)
(define-key erik-mode-map (kbd "C-o") 'other-window)
(define-key erik-mode-map (kbd "C-S-o")
  (lambda () (interactive) (other-window -1)))
(define-key erik-mode-map (kbd "C-c C-f") 'find-file-at-point)
(define-key erik-mode-map (kbd "<C-return>") 'electric-newline-and-maybe-indent)
(define-key erik-mode-map (kbd "C-x C-g") 'magit-status)
(define-key erik-mode-map (kbd "C-]") 'er/expand-region)
(define-key erik-mode-map (kbd "<C-M-backspace>") 'erik-collapse-line-back)
(define-key erik-mode-map (kbd "M-I") 'windmove-up)
(define-key erik-mode-map (kbd "M-K") 'windmove-down)
(define-key erik-mode-map (kbd "M-J") 'windmove-left)
(define-key erik-mode-map (kbd "M-L") 'windmove-right)

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

;; C-j t _ -- Tray / Terminal commands
(define-key erik-mode-map (kbd "C-j t s") 'erik-term-send-to-shell)
(define-key erik-mode-map (kbd "C-j t l") 'erik-term-run-last)
(define-key erik-mode-map (kbd "C-j t n") 'erik-next-term)
(define-key erik-mode-map (kbd "C-j t p") 'erik-prev-term)
(define-key erik-mode-map (kbd "C-j t t") 'erik-toggle-ansi-term)
(define-key erik-mode-map (kbd "C-j t o") 'ansi-term)
(define-key erik-mode-map (kbd "C-j t a") 'window-toggle-side-windows)
(define-key erik-mode-map (kbd "C-j t d") 'neotree-toggle)

;; C-j f _ -- Flymake
(define-key erik-mode-map (kbd "C-j f f") 'erik-flymake-eglot-toggle)
(define-key erik-mode-map (kbd "C-j f n") 'flymake-goto-next-error)
(define-key erik-mode-map (kbd "C-j f p") 'flymake-goto-prev-error)
(define-key erik-mode-map (kbd "C-j f a") 'flymake-show-project-diagnostics)
(define-key erik-mode-map (kbd "C-j f s") 'flymake-show-buffer-diagnostics)

;; C-j e _ -- Eval commands
(define-key erik-mode-map (kbd "C-j e e") 'eval-region)
(define-key erik-mode-map (kbd "C-j e p") 'erik-eval-and-print)
(define-key erik-mode-map (kbd "C-j e o") 'erik-eval-and-print-to-output)

;; C-j o _ -- org mode stuff
(define-key erik-mode-map (kbd "C-j o l") 'erik-md-to-org-link)
(define-key erik-mode-map (kbd "C-j o p") 'erik-org-publish-wiki)
(define-key erik-mode-map (kbd "C-j o s") 'org-slug-slugify)
(define-key erik-mode-map (kbd "C-j o t") 'org-slug-tagify)

;; C-j i _ -- input method commands
(define-key erik-mode-map (kbd "C-j i p") 'erik-input-pinyin)
(define-key erik-mode-map (kbd "C-j i c") 'erik-input-chinese)
(define-key erik-mode-map (kbd "C-j i e") 'erik-input-deactivate)

;; C-j h _ -- html commands
(define-key erik-mode-map (kbd "C-j h s n") 'erik-web-mode-slurp-node)
(define-key erik-mode-map (kbd "C-j h s t") 'erik-web-mode-slurp-text)

;; C-j c _ -- chinese commands
(define-key erik-mode-map (kbd "C-j c l c") 'erik-chinese-lookup-character)
(define-key erik-mode-map (kbd "C-j c l w") 'erik-chinese-lookup-word)

;; C-j a _ -- anki commands
(define-key erik-mode-map (kbd "C-j a c") 'erik-anki-make-cloze-buffer)
(define-key erik-mode-map (kbd "C-j a l") 'erik-anki-make-cloze-line)

;; Binding functions I didn't write
(define-key erik-mode-map (kbd "C-j C-j") 'ido-select-text)
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
;;; erik-mode.el
