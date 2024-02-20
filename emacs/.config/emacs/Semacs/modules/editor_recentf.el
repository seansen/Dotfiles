(use-package recentf
  ;; Keep track of recently opened files
  :commands recentf-open-files
  :config
  (defun semacs--recent-file-truename (file)
    (if (or (file-remote-p file nil t)
            (not (file-remote-p file)))
        (file-truename file)
      file))
    (setq recentf-filename-handlers
        '(;; Text properties inflate the size of recentf's files, and there is
          ;; no purpose in persisting them, so we strip them out.
          substring-no-properties
          ;; Resolve symlinks of local files. Otherwise we get duplicate
          ;; entries opening symlinks.
          semacs--recent-file-truename
          ;; Replace $HOME with ~, which is more portable, and reduces how much
          ;; horizontal space the recentf listing uses to list recent files.
          abbreviate-file-name)
        recentf-save-file (concat semacs-cache-dir "recentf")
        recentf-auto-cleanup 'never
        recentf-max-menu-items 0
        recentf-max-saved-items 200)
)
