
;;Vdiff let's one compare buffers or files.

(use-package vdiff
  :config
  ; This binds commands under the prefix when vdiff is active.
  (define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map))
