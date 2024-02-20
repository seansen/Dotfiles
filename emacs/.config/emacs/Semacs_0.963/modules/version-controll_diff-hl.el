
(use-package diff-hl
  :straight (:repo "dgutov/diff-hl" :host github :type git)
  :hook
  (prog-mode-hook . turn-on-diff-hl-mode)
  (vc-dir-mode-hook . turn-on-diff-hl-mode)
  :config
  (global-diff-hl-mode))
 ; (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
