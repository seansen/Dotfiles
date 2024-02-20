(use-package emacs
  :preface
  (defun kill-all-dired-buffers ()
    "Kill all dired buffers."
    (interactive)
    (save-excursion
      (let ((count 0))
        (dolist (buffer (buffer-list))
          (set-buffer buffer)
          (when (equal major-mode 'dired-mode)
            (setq count (1+ count))
            (kill-buffer buffer)))
        (message "Killed %i dired buffer(s)." count))))

  (defun kill-dired-buffers ()
    (interactive)
     (mapc (lambda (buffer)
             (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
               (kill-buffer buffer)))
           (buffer-list)))

  :bind (:map dired-mode-map ("h" . wsl/open-in-default-program ))
  :config
  ;; auto refresh dired when file changes
  (add-hook 'dired-mode-hook 'auto-revert-mode)

  :custom
  (dired-dwim-target t)
  (dired-guess-shell-alist-user '(("" "xdg-open")))
  (global-auto-revert-non-file-buffers t)
  (dired-find-file-other-buffer t)
  (auto-revert-verbose nil))
