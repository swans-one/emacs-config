(defun snake-to-camel (s)
  "Turn snake_case to CamelCase"
  (mapconcat 'capitalize (split-string s "_") ""))

(defun python-split-args (arg-string)
  "Split a python argument string into ((name, default)..) tuples"
  (mapcar (lambda (x)
             (split-string x "[[:blank:]]*=[[:blank:]]*" t))
          (split-string arg-string "[[:blank:]]*,[[:blank:]]*" t)))

(defun python-args-to-docstring ()
  "Return docstring format from the python arguments in yas-text"
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (python-split-args yas-text))
         (max-len (if args (apply 'max (mapcar (lambda (x) (length (nth 0 x))) args)) 0))
         (formatted-args (mapconcat
                (lambda (x)
                  (concat (nth 0 x) (make-string (- max-len (length (nth 0 x))) ? ) " -- "
                          (if (nth 1 x) (concat "\(default " (nth 1 x) "\)"))))
                args
                indent)))
    (unless (string= formatted-args "")
      (mapconcat 'identity (list "Keword Arguments:" formatted-args) indent))))

(defun python-args-to-rst-docstring ()
  "Return docstring in rst format from the python arguments in yas-text"
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (python-split-args yas-text))
         (arg-doc-items 
          (apply 'append (mapcar
                          (lambda (x)
                            `(,(concat ":param " (nth 0 x) ":")
                              ,(concat ":type " (nth 0 x) ":")
                              ""))
                          args)))
         (formatted-args (mapconcat 'identity arg-doc-items indent)))
    formatted-args))

(defun python-args-to-set-self ()
  "Return self.blah = blah for python arguments in yas-text"
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (python-split-args yas-text))
         (formatted-args (mapconcat
                          (lambda (x)
                            (concat "self." (nth 0 x) " = " (nth 0 x)))
                          args
                          indent)))
    formatted-args))

(add-hook 'python-mode-hook
          '(lambda () (set (make-local-variable 'yas-indent-line) 'fixed)))
