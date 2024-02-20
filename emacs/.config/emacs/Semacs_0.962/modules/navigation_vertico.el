;; Vertico
;; Provides a performant and minimalistic vertical completion UI

(use-package vertico
  :init
  (vertico-mode)

  :custom
  (vertico-resize t)
  (vertico-cycle t)

  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-precious)
         ("C-f" . vertico-exit)
         :map minibuffer-local-map
         ("M-h" . backward-kill-word)))