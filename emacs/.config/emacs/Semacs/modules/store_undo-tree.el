;;; UNDO-TREE

;;; Code
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (unbind-key "M-_" undo-tree-map))
