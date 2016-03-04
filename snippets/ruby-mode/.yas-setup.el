(defun yas-ruby-args-to-instance-vars ()
  (ruby-args-to-instance-vars (ruby-split-args yas-text)))

(defun ruby-args-to-instance-vars (args)
  (let ((indent (concat "\n" (make-string (current-column) 32))))
    (mapconcat 'identity
               (--map (concat "@" it " = " it indent) args)
               "")))

(defun yas-ruby-args-to-symbols ()
  (ruby-args-to-symbols (ruby-split-args yas-text)))

(defun ruby-args-to-symbols (args)
  (mapconcat (lambda (s) (concat ":" s)) args ", "))

(defun ruby-split-args (arg-string)
  "Split a string of ruby args into a list of argument names"
  (--map (replace-regexp-in-string
          ":.*" "" it)
         (split-string arg-string "\\(, ?\\)" t)))
