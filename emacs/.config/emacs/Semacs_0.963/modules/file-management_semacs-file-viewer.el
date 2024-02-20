
(defun semacs/visit-abbrev.org ()
  (interactive)
  (find-file (expand-file-name "snippets/semacs-abbrev.el" user-emacs-directory)))

(defun semacs/visit-templates ()
  (interactive)
  (find-file (expand-file-name "snippets/templates" user-emacs-directory)))

(defun semacs/visit-config.org ()
  (interactive)
  (find-file (expand-file-name "config.org" user-emacs-directory)))

(defun semacs/visit-init.el ()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun semacs/visit-roam-zettelkasten ()
  (interactive)
  (find-file org_notes))
