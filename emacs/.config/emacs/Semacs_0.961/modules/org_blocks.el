(use-package org
  :ensure nil
  :custom
  ;; Source blocks internally indented as they would normally in their majors modes.
  (org-src-tab-acts-natively t)
  ;; Non-nil means entering Org mode will fold all blocks.
  (org-hide-block-startup nil)
  ;; When non-nil, fontify code in code blocks.
  (org-src-fontify-natively t)
  ;; If non-nil preserve indentation.
  (org-src-preserve-indentation t))
