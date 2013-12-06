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

;;What file am I looking at.
(defun what-file () (interactive) (message (buffer-file-name)))

(provide 'defuns)
