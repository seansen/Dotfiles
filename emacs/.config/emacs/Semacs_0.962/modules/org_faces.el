(use-package org
  :ensure nil
  :config
  (custom-set-faces
   '(org-default         ((t (:family "Roboto Mono" :weight light ))))
  ; '(org-level-1        ((t (:inherit outline-1 :extend nil  :weight light))))
   '(org-level-1         ((t (:inherit outline-1 :extend nil :weight light :background "grey9" :overline "#123555"))))
   '(org-level-2         ((t (:inherit outline-1 :extend nil :weight light ))))
   '(org-level-3         ((t (:inherit outline-1 :extend nil :weight light))))
   '(org-level-4         ((t (:inherit outline-1 :extend nil :weight light))))
   '(org-level-5         ((t (:inherit outline-1 :extend nil :weight light))))
   '(org-level-6         ((t (:inherit outline-1 :extend nil :weight light))))
   '(org-level-7         ((t (:inherit outline-1 :extend nil :weight light))))
   '(org-tag             ((t (:height 1.0 :inherit outline-1 :extend nil :weight light :overline "#123555"))))
   '(org-link            ((t (:height 1.0 :underline t :inherit fixed-pitch :weight light ))))
   '(org-mode-line-clock ((t (:weight bold :box '(:color "#FFBB00")  :background "#FF4040"))))
   '(org-drawer          ((t (:height 1.0 :inherit fixed-pitch :weight light ))))
   '(org-indent          ((t (:height 1.0 :inherit fixed-pitch :weight light ))))
   '(org-block           ((t (:height 1.0 :inherit fixed-pitch :family "Roboto Mono" :weight light ))))
   '(org-code            ((t (:height 1.0 :inherit fixed-pitch :family "Roboto Mono" :weight light ))))
   '(org-roam-title      ((t (:height 1.0 :inherit fixed-pitch :weight light ))))
   '(org-block-begin-line     ((t (:height 1.0 :inherit fixed-pitch :family "Roboto Mono" :weight light ))))
   '(org-transclusione-fringe ((t (:weight bold :box '(:color "#FFBB00")  :background "#FF4040"))))))
