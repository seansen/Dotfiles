(setq company-ispell-dictionary (file-truename "~/.config/emacs/Semacs/german.txt"))
(setq company-ispell-dictionary (concat semacs-emacs-dir "german.txt"))

(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode))
