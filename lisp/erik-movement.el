;; Movement Functions
;;
;; Customized movement functions

(defun blank-line-up ()
  "Move up to the next blank line"
  (interactive)
  (re-search-backward "^\n" nil "start"))

(defun blank-line-down ()
  "Move down to the next blank line"
  (interactive)
  (forward-char)
  (when (re-search-forward "^\n" nil "start")
    (backward-char)))

(provide 'erik-movement)
