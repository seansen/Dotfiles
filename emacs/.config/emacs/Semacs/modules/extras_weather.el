(use-package wttrin
  :straight (:host github :repo "/etiago/emacs-wttrin"
             :branch "user-agent-fix")
  :ensure t
  :commands (wttrin)
  :init
  (setq wttrin-default-cities '("Weinheim"
                                "Flensburg")))
