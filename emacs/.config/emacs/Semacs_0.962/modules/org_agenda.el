(use-package org
  :ensure nil
  :config
  ;; Refresh org-agenda after rescheduling a task.
  (defun org-agenda-refresh ()
    "Refresh all `org-agenda' buffers."
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'org-agenda-mode)
          (org-agenda-maybe-redo)))))

  (defadvice org-schedule (after refresh-agenda activate)
    "Refresh org-agenda."
    (org-agenda-refresh))

  :custom
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-include-deadlines t)
  (org-agenda-block-separator #x2501)
  (org-agenda-compact-blocks t)
  (org-agenda-start-with-log-mode t)
  ;; Number of days before expiration during which a deadline becomes active.
  (org-deadline-warning-days 30)
  ;; If non-nil, include in the agenda entries from the Emacs Calendar's diary.
  ;; Don't log the time a task was rescheduled or redeadlined.
  ;(org-agenda-use-tag-inheritance t)
  (org-log-redeadline nil)
  (org-log-reschedule nil)
  (org-agenda-include-diary nil)
  (org-agenda-include-inactive-timestamps t)
  (org-agenda-start-with-clockreport-mode t)
  (org-agenda-start-with-log-mode nil)
  (org-agenda-show-outline-path nil)
  (org-agenda-show-all-dates nil)
  (org-agenda-start-on-weekday nil)
  (org-agenda-start-day "-3d")
  (org-agenda-span 10)
  (org-agenda-confirm-kill t)
  ;; Shift tags in agenda items to this column.
  (org-agenda-tags-column 107)
  (org-agenda-time-leading-zero t)
  (org-agenda-menu-show-matcher t)
  (org-agenda-menu-two-columns nil)
  (org-agenda-skip-comment-trees t)
  (org-agenda-sticky nil)
  (org-agenda-window-setup 'current-window)
  (org-agenda-custom-commands-contexts nil)
  (org-agenda-max-entries nil)
  (org-agenda-max-todos nil)
  (org-agenda-max-tags nil)
  (org-agenda-max-effort nil)
  (org-agenda-log-mode-add-notes t)
  (org-agenda-sort-notime-is-late t) ; Org 9.4
  (org-agenda-sort-noeffort-is-high t) ; Org 9.4
  (org-log-done (quote time))
  ;; Prefer rescheduling to future dates and times:
  (org-read-date-prefer-future 'time)

  ;; CUSTOM COMMANDS
  (org-agenda-custom-commands
   '(("d" "Dashboard"
      ((agenda "" ((org-deadline-warning-days 7)))
       (todo "INFO" ((org-agenda-overriding-header "Next Tasks")))
       (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

     ("n" "Next Tasks"
      ((agenda "" ((org-deadline-warning-days 7)))
       (todo "NEXT" ((org-agenda-overriding-header "Next Tasks")))))

     ("b" "Birtdays" tags-todo "+person")

     ("h" "Home Tasks" tags-todo "+home")

     ("w" "Work Tasks" tags-todo "+work")

     ("i" "ISB Tasks" tags-todo "+isb")

     ("E" "Easy Tasks" tags-todo "+easy")

     ("C" "Challenging Tasks" tags-todo "+challenging")

     ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
      ((org-agenda-overriding-header "Low Effort Tasks")
       (org-agenda-max-todos 20)
       (org-agenda-files org-agenda-files))))))
