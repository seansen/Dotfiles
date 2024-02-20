;;; Package

;;; Code:
(use-package auto-dim-other-buffers
  :custom-face
  (auto-dim-other-buffers-face
   ((t (:foreground "#65697e":background "#202020"))))
  (auto-dim-other-buffers-hide-face
   ((t (:background "black" :foreground "#65697e"))))

  :config
  (auto-dim-other-buffers-mode t))
