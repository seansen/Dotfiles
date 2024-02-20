(use-package org-treeusage
  :after org
  :bind ("C-c d" . org-treeusage-mode)
  :custom
  ((org-treescope-overlay-header nil)
   (org-treeusage-overlay-usecolorbands nil)))
