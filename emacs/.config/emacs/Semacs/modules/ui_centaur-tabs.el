(use-package centaur-tabs
  :straight (:repo "ema2159/centaur-tabs" :host github :type git)

  :preface
  (defun centaur-tabs-hide-tab (x)
    "Do no to show buffer X in tabs."
    (let ((name (format "%s" x)))
      (or
       ;; Current window is not dedicated window.
       (window-dedicated-p (selected-window))

       ;; Buffer name not match below blacklist.
       (string-prefix-p "*httpd*" name)
       (string-prefix-p "*dashboard*" name)
       (string-prefix-p "*ansi-term-1*" name)
       (string-prefix-p "*ansi-term-2*" name)
       (string-prefix-p "*epc" name)
       (string-prefix-p "*helm" name)
       (string-prefix-p "*Helm" name)
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*lsp" name)
       (string-prefix-p "*company" name)
       (string-prefix-p "*Flycheck" name)
       (string-prefix-p " *Messages*" name)
       (string-prefix-p "*Warnings*" name)
       (string-prefix-p "Treemacs" name)
       (string-prefix-p "OrgOutline" name)
       (string-prefix-p "*tramp" name)
       (string-prefix-p "diary" name)
       (string-prefix-p " *Mini" name)
       (string-prefix-p "*help" name)
       (string-prefix-p "*straight" name)
       (string-prefix-p " *temp" name)
       (string-prefix-p "*Help" name)
       (string-prefix-p "*mybuf" name)
       (string-prefix-p "*org-roam*" name)
       (string-prefix-p "*OrgOutline" name)

       ;; Is not magit buffer.
       (and (string-prefix-p "magit" name)
        (not (file-name-extension name))))))

  :config
  (add-hook 'dired-mode-hook 'centaur-tabs-local-mode)
  (add-hook 'shell-pop-out-hook 'centaur-tabs-local-mode)
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)

  :custom
  (centaur-tabs-show-count t)
  (centaur-tabs-style "wave")
  (centaur-tabs-height 22)
  (centaur-tabs-set-icons t)
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-set-bar 'left)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "*")
  (centaur-tabs-label-fixed-length 10))

