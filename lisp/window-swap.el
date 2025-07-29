;; Window swap
;;
;; Mark one window, and then another and swap the buffers in them.

(defvar window-swap-saved-window nil
  "When non nil means that window-swap has been called once.
This variable stores a reference to the window it was called in")

(defvar window-swap-saved-buffer nil
  "When non nil means that window-swap has been called once.
This variable stores a reference to the buffer it was called in")

(defun window-swap (window buffer)
  "Start or finish the window swap.

  Running this command the first time saves the current window
  and buffer. Running it a second time in a different window
  swaps the buffers in those windows."
  (interactive (list (selected-window) (current-buffer)))
  (if (and window-swap-saved-window window-swap-saved-buffer)
      (progn (do-window-swap window-swap-saved-window buffer
                             window window-swap-saved-buffer)
             (clear-window-swap))
    (progn (set-window-swap window buffer))))

(defun do-window-swap (window1 buffer1 window2 buffer2)
  "Set window1 to buffer1 and window2 to buffer2"
  (select-window window1 t)
  (switch-to-buffer buffer1 t t)
  (select-window window2 t)
  (switch-to-buffer buffer2 t t))

(defun clear-window-swap ()
  "Clear any saved window"
  (interactive)
  (setq window-swap-saved-window nil)
  (setq window-swap-saved-buffer nil))

(defun window-swap-start (window buffer)
  "Calls window-swap but first clears any previous saves.

   Use this if you're not sure if you're partway through an
   existing window swap to "
  (interactive (list (selected-window) (current-buffer)))
  (clear-window-swap)
  (window-swap window buffer))

(defun set-window-swap (window buffer)
  "Helper function to set the saved window and buffer"
  (setq window-swap-saved-window window)
  (setq window-swap-saved-buffer buffer))

(provide 'window-swap)
