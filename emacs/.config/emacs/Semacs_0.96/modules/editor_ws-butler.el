(use-package ws-butler
  ;; a less intrusive `delete-trailing-whitespaces' on save
  :config
  ;; ws-butler normally preserves whitespace in the buffer (but strips it from
  ;; the written file). While sometimes convenient, this behavior is not
  ;; intuitive. To the average user it looks like whitespace cleanup is failing,
  ;; which causes folks to redundantly install their own.
  (setq ws-butler-keep-whitespace-before-point nil))
