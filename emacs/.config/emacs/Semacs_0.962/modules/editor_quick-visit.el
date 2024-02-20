;;; PACKAGE --- CONTROLL_QUICK-VISIT"
;;;******************************************
;;; COMMENTARY:
;;; This file provides functions to quickly visit EMACS settings.
;;;******************************************

(defun semacs/visit-core-config ()
  (interactive)
  (find-file (expand-file-name "core/config.el" user-emacs-directory)))

(defun semacs/visit-core-settings ()
  (interactive)
  (find-file (expand-file-name "core/user-settings.el" user-emacs-directory)))
