
;; [#B] Dired-Sidebar

;; (use-package dired-sidebar
;;   :commands (dired-sidebar-toggle-sidebar)
;;   :init
;;     (add-hook 'dired-sidebar-mode-hook
;;       (lambda ()
;;         (unless (file-remote-p default-directory)
;;                 (auto-revert-mode))))

;;   :config
;;     (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
;;     (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
;;     (setq dired-sidebar-subtree-line-prefix "__")
;;     (setq dired-sidebar-use-custom-font t)
;;     (define-key dired-sidebar-mode-map (kbd "M-p") nil))


(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))
