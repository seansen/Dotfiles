;******************************************
; MOVE-LINE
;******************************************
(defun semacs/move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun semacs/move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "M-<down>") 'semacs/move-line-down)
(global-set-key (kbd "M-<up>") 'semacs/move-line-up)

