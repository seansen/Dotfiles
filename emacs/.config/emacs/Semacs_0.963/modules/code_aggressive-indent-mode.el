;;; package --- Summary
;;; Commentary:
;;; Code:

(straight-use-package
 '(aggressive-indent-mode :type git :host github :repo "Malabarba/aggressive-indent-mode")
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'css-mode-hook #'aggressive-indent-mode))

(provide 'code_aggressive-indent-mode)

;;; code_aggressive-indent-mode.el ends here
