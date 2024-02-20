
(use-package smartparens
  :init
  (smartparens-global-mode)
  :config
  (add-hook 'text-mode-hook 'smartparens-mode)
  (add-hook 'prog-mode-hook 'smartparens-mode))
