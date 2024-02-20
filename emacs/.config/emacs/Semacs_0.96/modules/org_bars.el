(use-package org-bars
  :straight (org-bars :type git :host github :repo "tonyaldon/org-bars")
  ;:after org
  :hook
  (org-mode . org-bars-mode)

  :config
;  (setq org-bars-stars '(:empty "⭕":invisible "㊗":visible "🔴"))
  (setq org-bars-stars '(:empty "○":invisible "◉":visible "◉"))

  :custom
  (org-bars-color-options nil)
  (org-bars-color-options '(:only-one-color t
                            :bar-color "white")))

