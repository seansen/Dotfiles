;;; package --- org_export

;;; commentary:
;; This file

;;; Code:

;;; org_export.el ends here
(use-package org
  :ensure nil
  :custom
  ;(org-html-htmlize-output-type 'inline-css)
  (org-html-htmlize-output-type 'css)
  ;; If non-nil preserve leading whitespace characters on export.
  (org-src-preserve-indentation t))


(defun org-export-output-file-name-modified (orig-fun extension &optional subtreep pub-dir)
  (unless pub-dir
    (setq pub-dir "~/Org/org-export")
    (unless (file-directory-p pub-dir)
      (make-directory pub-dir)))
  (apply orig-fun extension subtreep pub-dir nil))
(advice-add 'org-export-output-file-name :around #'org-export-output-file-name-modified)