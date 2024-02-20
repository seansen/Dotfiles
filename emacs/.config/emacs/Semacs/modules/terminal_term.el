;;; Term
;;Term-Mode is a built-in terminal emulator in Emacs.
(use-package term
  :config
  (setq explicit-shell-file-name "zsh") ;; Change this to zsh, etc
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))
