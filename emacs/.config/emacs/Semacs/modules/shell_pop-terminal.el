
;;; Shell-Pop

(use-package shell-pop
  :straight (:repo "kyagi/shell-pop-el"
                   :host github
                   :type git)

  :custom
  (shell-pop-default-directory "~/" Default directory)
  (shell-pop-shell-type
    (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (shell-pop-term-shell "/bin/zsh")
  (shell-pop-universal-key "C-a")
  (shell-pop-window-size 30)
  (shell-pop-full-span t)
  (shell-pop-window-position "bottom")
  (shell-pop-autocd-to-working-dir t)
  (shell-pop-restore-window-configuration t)
  (shell-pop-cleanup-buffer-at-process-exit t))
