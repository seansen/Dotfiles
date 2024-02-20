
;; Dired-Open
(use-package dired-open
  :custom
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (dired-open-extensions '(("png" . "feh")
                           ("mkv" . "mpv"))))
