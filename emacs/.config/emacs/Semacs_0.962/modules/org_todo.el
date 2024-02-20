;; Org-Todo

(use-package org
  :ensure nil
  :custom
  ;; Fast todo selection
  (org-use-fast-todo-selection t)
  ;; Non-nil means undone TODO entries will block switching the parent to DONE.
  (org-enforce-todo-dependencies t)
  ;; Non-nil means unchecked boxes will block switching the parent to DONE.
  (org-enforce-todo-checkbox-dependencies t)
  ;; Non-nil keeps tags aligned when modifying headlines.
  (org-auto-align-tags t)
  ;; List of TODO entry keyword sequences and their interpretation.
  (org-todo-keywords
     (quote ((sequence "TODO(t)" "NEXT(n)" "CLOCK" "PROG" "|" "DONE(d)" "CANCELLED")
             (sequence "PROJ" "INFO" "WAIT" "MEETING" "|" "DONE" "CANCELLED"))))

  ;; Faces for specific TODO keywords.
  (org-todo-keyword-faces
    '(("TODO"      :background "yellow" :weight bold :foreground "black")
      ("CLOCK"     :background "magenta" :weight bold :foreground "black")
      ("NEXT"      :background "red" :weight bold :foreground "black")
      ("PROG"      :background "orange" :weight bold :foreground "black")
      ("PROJ"      :background "pink4" :weight bold :foreground "black")
      ("DONE"      :background "green" :weight bold :foreground "black")
      ("INFO"      :background "cornflower blue" :weight bold :foreground "black")
      ("WAIT"      :background "brown" :weight bold :foreground "white")
      ("MEETING"   :background "gold" :weight bold :foreground "black")
      ("CANCELLED" :background "dark red" :weight bold :foreground "white"))))


