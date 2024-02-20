(use-package org-appear
  :straight (org-appear :type git :host github :repo "awth13/org-appear")
  :hook (org-mode . org-appear-mode)
  :init
  (setq org-appear-delay 0.9)
  (setq org-appear-autoemphasis  t)
  (setq org-appear-autolinks t)
  (setq org-hide-emphasis-markers t)
  (setq org-appear-autosubmarkers t))
