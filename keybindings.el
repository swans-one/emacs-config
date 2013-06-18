;;
;; All global changes to keybindings go in this section.
;;

;; Switch windows with one keystroke
(global-set-key (kbd "C-o") 'other-window)

;; Magit-status
(global-set-key (kbd "C-x C-g") 'magit-status)

;; Insert a dollar sign pair
(global-set-key (kbd "C-c 4") 'insert-dollar-pair)

(provide 'keybindings)
