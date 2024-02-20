;; Cli

(use-package emacs
  :config
  (display-time-mode 1)
  ;;
  (display-battery-mode 1)
  ;;
  (line-number-mode 1)
  ;;
  (column-number-mode 1)
  ;;
  (size-indication-mode 1)
  ;;
  (minibuffer-depth-indicate-mode)

  :custom
  ;; Please update the time every second.
  (display-time-interval 1)

  ;; Display time, day and date.
  (display-time-day-and-date t)

  ;; Diesplay 24hr format.
  (display-time-24hr-format t)

  ;; E.g.,:  Fri Mar 04 ╱ 03:42:08 pm
  (display-time-format "%a %b %d ╱ %T")

  ;;
  (minibuffer-depth-indicate-mode)

  ; I don't need the system load average in the modeline.
  (display-time-default-load-average nil)
  (display-time-load-average nil))
