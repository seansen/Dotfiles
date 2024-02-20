(use-package org
  :ensure nil
  :config
  (org-clock-persistence-insinuate)                                           ;

  ;; Resume clocking task when emacs is restarted
  (org-clock-persistence-insinuate)

  (set-face-attribute 'org-mode-line-clock nil
     :weight 'bold :box '(:color "#FFBB00") :foreground "white" :background "#FF4040")


  :custom
  ;; Show lot of clocking history
  (org-clock-history-length 23)
  ;; Resume clocking task on clock-in if the clock is open
  (org-clock-in-resume t)
  ;; Sometimes I change tasks I'm clocking quickly ---this removes clocked tasks with 0:00 duration
  (org-clock-out-remove-zero-time-clocks t)
  ;; Clock out when moving task to a done state
  (org-clock-out-when-done t)
  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (org-clock-persist t)
  ;; Do not prompt to resume an active clock
  (org-clock-persist-query-resume nil)
  ;; Include current clocking task in clock reports
  (org-clock-report-include-clocking-task t)
  ;; Include current clocking task in clock reports
  (org-clock-report-include-clocking-task t)
  ;;
  (org-clock-persist-file (expand-file-name "clock-persist.el" semacs-cache-dir))
  ;; Clock sound file
  (org-clock-sound (expand-file-name "semacs-sound1.wav" semacs-etc-dir))
  ;; Clock out when moving task to a done state
  (org-clock-auto-clock-resolution (quote when-no-clock-is-running))

  (org-clock-persist 'history)                                           ;
  (org-clock-idle-time 15)
  (org-clock-mode-line-total 'current)
  (org-clock-clocked-in-display 'both)
  (org-clock-in-switch-to-state "CLOCK")
  (org-clock-out-switch-to-state "TODO")
  (org-clock-sound t)
  (org-clock-into-drawer t)
  (org-clock-out-remove-zero-time-clocks t)
  (org-pretty-entities t)
  (org-clock-persist-query-resume t)
  (org-log-into-drawer t)                                                ;
  (org-log-note-clock-out t))