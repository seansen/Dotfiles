(use-package helpful
 :config
 ;;Open linked entire in same Help-Buffer window.
 (setq helpful-switch-buffer-function #'+helpful-switch-to-buffer)

  (defun +helpful-switch-to-buffer (buffer-or-name)
    "Switch to helpful BUFFER-OR-NAME.
     The logic is simple, if we are currently in the helpful buffer,
     reuse it's window, otherwise create new one."
     (if (eq major-mode 'helpful-mode)
         (switch-to-buffer buffer-or-name)
       (pop-to-buffer buffer-or-name))))
