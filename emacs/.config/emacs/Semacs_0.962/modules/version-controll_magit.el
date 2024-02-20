(use-package magit
  :commands
  magit-status

  :init
  ;; We do this ourselves further down
  (setq magit-auto-revert-mode nil)

  :custom
  (transient-default-level 5)

  ;; Show granular diffs in selected hunk
  (magit-diff-refine-hunk t)

  ;; Don't autosave repo buffers. This is too magical, and saving can
  ;; trigger a bunch of unwanted side-effects, like save hooks and
  ;; formatters. Trust the user to know what they're doing.
  (magit-save-repository-buffers nil)

  ;; Don't display parent/related refs in commit buffers; they are rarely
  ;; helpful and only add to runtime costs.
  (magit-revision-insert-related-refs nil))
