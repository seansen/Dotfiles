(use-package emojify
  :hook
  (after-init . global-emojify-mode)

  :config
  (when (member "Segoe UI Emoji" (font-family-list))
    (set-fontset-font t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend))

  (setq emojify-display-style 'unicode)
  (setq emojify-emoji-styles '(unicode))
  (bind-key* (kbd "C-c .") #'emojify-insert-emoji))
