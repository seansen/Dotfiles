(use-package org-appear
  :defer t
  :straight (org-appear :type git :host github :repo "awth13/org-appear")
  :hook (org-mode . org-appear-mode)
  :init
  (setq org-appear-autoemphasis  t)
  ;; (setq org-appear-autolinks t)
  (setq org-appear-autosubmarkers t))
