
(use-package neotree
  :init
  (require 'neotree)
 :config
   (setq neo-autorefresh nil)
   (setq neo-window-width 45)
   (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
   (setq neo-smart-open t))
(provide 'init-neotree)
(global-set-key [f8] 'neotree)
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "x") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
  (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
  (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
  (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "h") 'neotree-hidden-file-togglf)
