;;; (package manager)

;;; Code

;;; Package.el
;;  - Ensure that, if we do need package.el, it is configured correctly.
(setq package-enable-at-startup nil
      package-user-dir (concat semacs-local-dir "elpa/"))

(require 'package)

(setq my-package-archives '(("melpa-stable" . "http://stable.melpa.org/packages/")
;                            ("org" . "http://orgmode.org/elpa/")
                            ("melpa" . "https://melpa.org/packages/")))

(setq package-archives (append package-archives
                               my-package-archives))


;;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;;; Replace use-package with straight-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
