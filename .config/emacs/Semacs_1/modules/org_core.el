;******************************************
; ORG-CONFIG
;******************************************

;; Org Init
(when (eq system-type 'gnu/linux)
(setq user-org-directory (concat (getenv "HOME") "/Org/"))
(setq org_notes (expand-file-name "zettelkasten/" user-org-directory))
(setq org_agenda (expand-file-name "zettelkasten/task.org" user-org-directory))
(setq org_bib (expand-file-name "zettelkasten/bibnotes.org" user-org-directory))
(setq org_export (expand-file-name "zettelkasten/org-export/" user-org-directory))
(setq zot_bib (expand-file-name "zettelkasten/MeineBibliothek.bib" user-org-directory))
)

;Quickly visit Zettelkasten
(defun saa/visit-roam-zettelkasten ()
  (interactive)
  (find-file "~/Org/zettelkasten/index.org"))

;Org-Settings
(use-package org
  :init
  (progn
    (setq org-directory user-org-directory)
    (setq org-log-done 'time)
    (setq org-completion-use-ido t))
  :config
    (setq org-return-follows-link t)              ;Open Links with =RET=
    (setq org-drawers (quote ("PROPERTIES" "CLOCKTABLE" "LOGBOOK" "CLOCK")))
    (setq org-startup-folded 'fold)
    (setq org-cycle-separator-lines 0)             ;Structure the collapsed view
    (setq org-agenda-start-with-log-mode t)
    (setq org-log-into-drawer t)
    (setq org-ellipsis " ")
    (setq org-startup-indented t)                  ;Indent according to section
    (setq org-startup-with-inline-images t)        ;Display images in-buffer by default
    (setq org-link-frame-setup
      '((file . find-file)))                       ;Open Org-Link in same buffer
    (setq org-src-tab-acts-natively t)
    (setq org-src-preserve-indentation t)
    (setq org-todo-keywords
      '((sequence "TODO" "FEEDBACK" "VERIFY" "CANCELLED" "NOTINUSE" "|" "DONE" "DELEGATED")))
    (setq org-todo-keyword-faces
      '(("TODO" :foreground "#7c7c75" :weight normal :underline t)
        ("FEEDBACK" :foreground "#9f7efe" :weight normal :underline t)
        ("VERIFY" :foreground "#0098dd" :weight normal :underline t)
        ("DONE" :foreground "#50a14f" :weight normal :underline t)
        ("NOTINUSE" :foreground "#50a14f" :weight normal :underline t)
        ("DELEGATED" :foreground "#B5123E" :weight normal :underline t)
        ("CANCELLED" :foreground "#ff6480" :weight normal :underline t)))
    (setq org-priority-faces
      '((65 :foreground "#e45649")
        (66 :foreground "#da8548")
        (67 :foreground "#0098dd"))))

;; Font size
(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.0)
                (org-level-6 . 1.0)
                (org-level-7 . 1.0)
                (org-level-8 . 1.0)))
        (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

;;
(custom-theme-set-faces
  'user
  '(org-block ((t (:inherit fixed-pitch))))
  '(org-code ((t (:inherit (shadow fixed-pitch)))))
  '(org-document-info ((t (:foreground "dark orange"))))
  '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
  '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
  '(org-link ((t (:foreground "royal blue" :underline t))))
  '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  '(org-property-value ((t (:inherit fixed-pitch))) t)
  '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
  '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
  '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
