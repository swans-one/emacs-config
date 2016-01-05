;;; Compiled snippets and support files for `python-mode'
;;; .yas-setup.el support file if any:
;;;
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
;;; Snippet definitions:
;;;
(yas-define-snippets 'python-mode
                     '(("ae" "self.assertEqual(${1:result}, ${2:expected})" "assertEqual" nil nil nil nil nil nil)
                       ("cls" "class $1(object):\n    \"\"\"$2\n    \"\"\"\n    def __init__(self, $3):\n        \"\"\"\n        ${3:$(python-args-to-rst-docstring)}\n        \"\"\"\n        ${3:$(python-args-to-set-self)}" "class" nil nil nil nil nil nil)
                       ("def" "def $1($2):\n    \"\"\"$3\n\n    ${2:$(python-args-to-rst-docstring)}\n    :rtype:\n    :returns:\n    \"\"\"\n    ${0:pass}" "function" nil nil nil nil nil nil)
                       ("defp" "def _$1($2):\n    \"\"\"$3\n    \"\"\"\n    ${0:pass}" "privatefunction" nil nil nil nil nil nil)
                       ("model" "class $1(models.Model):\n    \"\"\"$2\n    \"\"\"\n    $0" "djangoModel" nil nil nil nil nil nil)
                       ("djtest" "class $1Test(TestCase):\n    def setUp(self):\n        pass\n\n    def test_$2(self):\n        pass" "djangoTest" nil nil nil nil nil nil)
                       ("fk" "$1 = models.ForeignKey('${1:$(snake-to-camel yas-text)}')\n$0" "foreignKey" nil nil nil nil nil nil)
                       ("md" "def $1(self$2):\n    \"\"\"$3\n\n    ${2:$(python-args-to-rst-docstring)}\n    :rtype:\n    :returns:\n    \"\"\"\n    ${0:pass}" "method" nil nil nil nil nil nil)
                       ("mdp" "def _$1(self$2):\n    \"\"\"$3\n    \"\"\"\n    ${0:pass}" "privateMethod" nil nil nil nil nil nil)
                       ("p" "print($0)" "print" nil nil nil nil nil nil)
                       ("test" "class $1Test(unittest.TestCase):\n    def setUp(self):\n        pass\n\n    def test_$2(self):\n        pass" "test" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Wed Dec  9 23:13:56 2015
