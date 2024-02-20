;******************************************
; CLIPMON
;******************************************
(use-package clipmon
  :defer t
  :init
    (add-to-list 'after-init-hook 'clipmon-mode-start))
