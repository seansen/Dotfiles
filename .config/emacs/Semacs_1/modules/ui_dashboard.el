(use-package dashboard
   :config
    (dashboard-setup-startup-hook)

    (setq dashboard-startup-banner (expand-file-name "apperance-dashboard-lion.png" semacs-modules-dir))
    (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
    ;(setq dashboard-startup-banner 2)
;; TEST
    (defun dashboard-insert-custom (list-size)
    (insert "Custom text"))
    (add-to-list 'dashboard-item-generators  '(custom . dashboard-insert-custom))
    (add-to-list 'dashboard-items '(custom) t)

;; Edits
    (setq dashboard-banner-logo-title "Welcome to Semacs!")
    (setq dashboard-init-info "Your pain is the breaking of the shell that encloses your understanding.")
    (setq dashboard-show-shortcuts nil)
    ;(setq dashboard-startup-banner 'official)
    ;; Content is not centered by default. To center, set
    (setq dashboard-center-content t)
    (setq dashboard-set-init-info t)
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-navigator t)
    (setq dashboard-set-file-icons t)
    (setq dashboard-items '((recents . 10)
                            (projects . 5)
                            (bookmarks  . 5)
                            (agenda    . 5))))

(provide 'apperance-dashboard)

;;; apperance-dashboard ends here
