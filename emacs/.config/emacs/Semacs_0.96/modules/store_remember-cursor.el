;******************************************
; UNDO-FU
;******************************************

(setq save-place-file
      (expand-file-name "saveplace" semacs-cache-dir))
(if (version<= emacs-version "25.1")
    (progn
      (setq-default save-place t)
      (require 'saveplace))
  (save-place-mode 1))




