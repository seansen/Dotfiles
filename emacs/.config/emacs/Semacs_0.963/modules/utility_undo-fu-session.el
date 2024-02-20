(use-package undo-fu-session
  ;:hook
  ;(undo-fu-mode . global-undo-fu-session-mode)
  :config
  ;(setq undo-fu-session-directory (concat semacs-cache-dir "undofu/"))
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(global-undo-fu-session-mode)