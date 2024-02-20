
;; Dired-Hide-Dotfiles
(use-package dired-hide-dotfiles
  :config
  (dired-hide-dotfiles-mode)
  (define-key dired-mode-map "." 'dired-hide-dotfiles-mode))
