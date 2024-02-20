(use-package undo-fu
  :config
  (setq undo-limit 400000
        undo-strong-limit 3000000
        undo-outer-limit 3000000)
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo))