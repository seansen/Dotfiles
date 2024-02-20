(use-package org-fancy-priorities
; :disabled
 :after org
 :hook
 (org-mode . org-fancy-priorities-mode)
 :custom
  (org-fancy-priorities-list '("A" "B" "C" "D" "E" "F")))
;  (org-fancy-priorities-list '("🚫" "🔨" "✅" "🇩" "🇪" "🇫")))
