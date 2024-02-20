(use-package company
:config
(setq company-selection-wrap-around t
      company-idle-delay 0.5
      company-minimum-prefix-length 2
      company-show-numbers t)
(add-hook 'evil-normal-state-entry-hook #'company-abort) ;; make aborting less annoying.
;===========================================
(company-tng-configure-default)
(global-company-mode t)
)


(use-package company-php
  :defer
  :after company)
