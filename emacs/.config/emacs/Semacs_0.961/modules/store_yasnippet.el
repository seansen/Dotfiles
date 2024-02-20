(use-package yasnippet
  :init
  (yas-global-mode)
  :config
;    (setq yas-snippet-dirs (concat semacs-emacs-dir "snippets"))
    (setq yas-snippet-dirs '("~/Demacs/snippets"))
    ;(setq yas-snippet-dirs '((expand-file-name "snippets" semacs-emacs-dir)))
    (yas-global-mode 1)
    (use-package yasnippet-snippets)
    (yas-reload-all))


(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand)
