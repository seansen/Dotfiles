(add-to-list 'custom-theme-load-path (concat user-emacs-directory
                                             "themes/"))
;; some themes have several variations (e.g. light and dark)
;; and share code between these variations in common elisp modules;
;; these modules need to be on the load path so that these themes work
(add-to-list 'load-path (concat user-emacs-directory
                                "themes/"))

;; Remember-Last-Theme
(use-package remember-last-theme
  :config
    ;(remember-last-theme-enable)
     (remember-last-theme-with-file-enable "~/Demacs/.local/cache/remember-theme.el"))

(setq custom-safe-themes t)

;; Doom-Themes
(use-package doom-themes)
