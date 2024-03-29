(use-package web-mode
  :defer t
  :config
    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
  )

;(defun my-web-mode-hook ()
;  "Hooks for Web mode."
;  (setq web-mode-markup-indent-offset 2)
;  (setq web-mode-code-indent-offset 2)
;  (setq web-mode-css-indent-offset 2)
;)
;(add-hook 'web-mode-hook  'my-web-mode-hook)
;(setq tab-width 2)
;
;(setq web-mode-enable-current-column-highlight t)
;(setq web-mode-enable-current-element-highlight t)
