;******************************************
; DOOM-MODELINE
;******************************************

(use-package doom-modeline
  :after eshell     ;; Make sure it gets hooked after eshell
  :custom
  (setq doom-modeline-height 15)
  (setq doom-modeline-bar-width 6)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-github nil)
  (setq doom-modeline-mu4e nil)
  (setq doom-modeline-irc nil)
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-persp-name nil))

(add-hook 'after-init-hook #'doom-modeline-mode)

; UTF-8 don’t show in modeline unless the encoding is something different
(defun doom-modeline-conditional-buffer-encoding ()
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))
(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)
