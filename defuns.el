;;
;; Individual functions I've written
;;

(defun insert-dollar-pair ()
  (interactive)
  (insert "$$")
  (backward-char 1)
)

(provide 'defuns)
