
;; Dired-Subtree
(use-package dired-subtree
  :config
    (setq dired-subtree-use-backgrounds nil)
    (let ((map dired-mode-map))
    (define-key map (kbd "<tab>") #'dired-subtree-toggle)
    (define-key map (kbd "<C-tab>") #'dired-subtree-cycle)
    (define-key map (kbd "<backtab>") #'dired-subtree-remove))
    (bind-keys :map dired-mode-map
      ("i" . dired-subtree-insert)
      (";" . dired-subtree-remove)))
