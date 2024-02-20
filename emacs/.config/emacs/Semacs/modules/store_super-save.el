;******************************************
; SUPER-SAVE
;******************************************

(use-package super-save
  :config
  (super-save-mode +1))
  
;; add integration with ace-window

(add-to-list 'super-save-triggers 'ace-window)
;; save on find-file
(add-to-list 'super-save-hook-triggers 'find-file-hook)



