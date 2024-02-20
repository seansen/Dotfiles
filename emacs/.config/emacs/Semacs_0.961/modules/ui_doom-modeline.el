;******************************************
; DOOM-MODELINE
;******************************************

(use-package doom-modeline
  :after
  eshell     ;; Make sure it gets hooked after eshell

  :init
  (doom-modeline-mode 1)

  :config
  (doom-themes-visual-bell-config)

  :custom
  (doom-modeline-height 35)
  (doom-modeline-buffer-state-icon nil)
  (doom-modeline-bar-width 1)
  (doom-modeline-hud t)
  ;; Show 3 Flycheck numbers: “red-error / yellow-warning / green-info”
  (doom-modeline-checker-simple-format nil)
  (doom-modeline-minor-modes t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-enable-word-count t))

  (add-hook 'after-init-hook #'doom-modeline-mode)

  ; UTF-8 don’t show in modeline unless the encoding is something different
  (defun doom-modeline-conditional-buffer-encoding ()
    (setq-local doom-modeline-buffer-encoding
                (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                            (eq buffer-file-coding-system 'utf-8)))))
    (add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)
