(use-package org-fancy-priorities
  :after org
  :hook
    (org-mode . org-fancy-priorities-mode)
  :config
    (setq org-fancy-priorities-list '("\u2764" "\u2708" "\u2706" "test" "\u270e")))
