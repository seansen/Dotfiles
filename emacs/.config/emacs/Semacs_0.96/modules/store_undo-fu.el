(use-package undo-fu
  :config
  (global-undo-tree-mode -1)
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo))

(use-package undo-fu-session
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (setq undo-fu-session-directory (concat semacs-cache-dir "undofu/"))
  (add-hook 'org-mode-hook
  (lambda ()
    (undo-fu-session-mode)))
)



;;; iiii
