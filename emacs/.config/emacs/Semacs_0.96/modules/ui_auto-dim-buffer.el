;;; Package

;;; Code:
(use-package auto-dim-other-buffers
  :config
  (require 'auto-dim-other-buffers)
  (add-hook 'after-init-hook (lambda ()
    (when (fboundp 'auto-dim-other-buffers-mode)
    (auto-dim-other-buffers-mode t)))))
