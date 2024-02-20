
(use-package affe
  :straight  (:repo "minad/affe" :host github :type git)

  :config
  (consult-customize affe-grep :preview-key (kbd "M-.")))
