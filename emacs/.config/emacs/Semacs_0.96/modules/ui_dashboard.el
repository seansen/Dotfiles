(use-package dashboard
  :preface
  (defun my/dashboard-banner ()
    "Set a dashboard banner including information on package initialization time and garbage collections."""
    (setq dashboard-banner-logo-title
          (format "Emacs ready in %.2f seconds with %d garbage collections."
                  (float-time (time-subtract after-init-time before-init-time)) gcs-done)))

  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  (add-hook 'dashboard-mode-hook 'my/dashboard-banner)

  :config
  (dashboard-setup-startup-hook)

  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

 ; (setq dashboard-startup-banner (expand-file-name "semacs_logo.png" semacs-etc-dir))
  (setq dashboard-startup-banner (expand-file-name "apperance-semacs_logo.png" semacs-modules-dir))
  
  (setq dashboard-banner-logo-title "Welcome to Semacs!")

  (setq dashboard-show-shortcuts t)

  (defun dashboard-insert-custom (list-size)
  (insert "Custom text"))

  (setq dashboard-week-agenda nil)
  (setq dashboard-filter-agenda-entry 'dashboard-filter-no-agenda)
 ;(add-to-list 'dashboard-items '(agenda) nil)

  (add-to-list 'dashboard-item-generators  '(custom . dashboard-insert-custom))
  (add-to-list 'dashboard-items '(custom) t)
  ;; A randomly selected footnote will be displayed. To disable it:
  (setq dashboard-set-footer nil)
  (setq dashboard-footer-messages '("Dashboard is pretty cool!"))

  (setq dashboard-init-info
    (concat "Emacs "emacs-version" Org-mode " org-version))
  ;(setq dashboard-startup-banner 'official)

  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)
  (setq dashboard-set-init-info t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-navigator t)
  ;(setq dashboard-footer "Pain is the game")
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents . 5)
                            (projects . 5)
                            (bookmarks  . 2)
                            (agenda    . 9))))

(provide 'apperance-dashboard)

;;;Dashboard in Emacsclient

;;;This setting ensures that emacsclient always opens on dashboard rather than scratch.
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;;; apperance-dashboard ends here