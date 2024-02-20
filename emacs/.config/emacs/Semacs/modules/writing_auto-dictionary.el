(use-package auto-dictionary
  :config
  (add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1))))
