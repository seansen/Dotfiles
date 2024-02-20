;******************************************
; GO-TRANSLATE
;******************************************

(use-package go-translate
  :config
    (setq go-translate-token-current (cons 430675 2721866130))
    (setq go-translate-buffer-follow-p t)   
    (setq go-translate-local-language "de")
    (setq go-translate-target-language "en")
    (setq go-translate-extra-directions '(("en" . "de"))))


(add-hook 'go-translate-after-render-hook
          (defun your-hook-that-disable-evil-mode-in-go-translate-buffer (&rest _)
            (turn-off-evil-mode -1)))
