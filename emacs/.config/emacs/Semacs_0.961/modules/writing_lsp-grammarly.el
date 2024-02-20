(use-package lsp-grammarly
  :hook
    (text-mode . (lambda ()
    (require 'lsp-grammarly)
    (lsp))))
