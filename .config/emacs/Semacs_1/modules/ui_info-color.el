;******************************************
; INFO-COLOR
;******************************************

(use-package info-colors
  :defer t 
  :commands (info-colors-fontify-node))
  (add-hook 'Info-selection-hook 'info-colors-fontify-node)
  (add-hook 'Info-mode-hook #'mixed-pitch-mode)


