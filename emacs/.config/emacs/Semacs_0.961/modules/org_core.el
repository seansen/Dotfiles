;******************************************
; ORG-CONFIG
;******************************************

;; Org Init
(setq user-org-directory (concat (getenv "HOME") "/Org/"))
;; My zettelkasten directory
(setq org_zettelkasten (expand-file-name "Zettelkasten/" user-org-directory))
;; Org notes files
(setq org_notes (expand-file-name "Zettelkasten/notes.org" user-org-directory))
;; Org bib notes files
(setq org_bib (expand-file-name "Zettelkasten/bibnotes.org" user-org-directory))
;; Org export directory
(setq org_export (expand-file-name "Zettelkasten/org-export/" user-org-directory))
;; Bib file
(setq zot_bib (expand-file-name "Zettelkasten/MeineBibliothek.bib" user-org-directory))
;; Org default note file
(setq org-default-notes-file (expand-file-name "Zettelkasten/todo.org" user-org-directory))
;; Org archive file
(setq org-archive-location "~/Org/Zettelkasten/archive/archive.org::* From %s")
;; Configure attachments to be stored together with their Org document.
(setq org-attach-id-dir (expand-file-name "Zettelkasten/attachments/" user-org-directory))
;; Org agenda files
(setq org-agenda-files
  '("~/Org/Zettelkasten/todo.org"
    "~/Org/Zettelkasten/contacts.org"
    ;"~/Org/Zettelkasten/inbox.org"
    "~/Org/Zettelkasten/recurrent.org"))

(use-package org
  :ensure org-plus-contrib
  :hook
  ((org-mode . visual-line-mode)
   (org-mode . which-function-mode)
   (org-mode . turn-on-auto-fill)
   (org-mode . auto-dictionary-mode)
   (org-mode . org-indent-mode)
   (org-mode . global-prettify-symbols-mode)
   (org-mode . prettify-symbols-mode)
   (org-mode . visual-line-mode))

  :init
  (progn
    ;; Set org directory
    (setq org-directory user-org-directory)
    ;;
    (setq org-completion-use-ido nil)
    ;; Information to record when a task moves to the DONE state.
    (setq org-log-done 'time))

    (defun my/update-completions-list ()
        (progn
            (push #'cape-dict completion-at-point-functions)
            (push #'cape-dabbrev completion-at-point-functions)
            (push #'cape-file completion-at-point-functions)))

  :config
  (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
  (define-key org-mode-map (kbd "C-,") nil)

  :custom
  ;;;??????
  ;(epa-pinentry-mode 'loopback)
  ;; Check if in invisible region before inserting or deleting a character.
  (org-catch-invisible-edits 'show-and-error)
  ;; Number of empty lines needed to keep an empty line between collapsed trees.
  (org-cycle-separator-lines 0)
  ;; If nil, the new heading is created directly after the current line.
  (org-insert-heading-respect-content t)
  ;; If non-nil make TAB cycle visibility on plain list items.
  (org-cycle-include-plain-lists 'integrate)
  ;; If non-nil means open Links with =RET=
  (org-return-follows-link t)
  ;; If non-nil means M-RET will split the line at the cursor position.
  (org-M-RET-may-split-line nil)
  ;; If non-nil means make shift-cursor commands select text when possible.
  (org-support-shift-select t)                                           ;
  ;; Open Org-Link in same buffer
  (org-link-frame-setup '((file . find-file)))
  ;; If non-nil lists may be labelled with letters.
  (org-list-allow-alphabetical t)
  ;; The ellipsis to use in the Org mode outline.
  (org-ellipsis " ...")
  ;(org-ellipsis "                ...")
  ;(org-ellipsis "                                         📎 ")
  ;; Non-nil means entering Org mode will switch to OVERVIEW.
  (org-startup-folded 'fold)                                             ;
  ;; When non-nil indent text according to outline structure.
  (org-startup-indented t)
  ;; Display images in-buffer by default
  (org-startup-with-inline-images t)
  ;; Non-nil means interpret "_" and "^" for display.
  (org-use-sub-superscripts '{})
  ;; Give quote and verse blocks a nice look.
  (org-fontify-quote-and-verse-blocks t)
  ;; Display links as the description provided
  (org-descriptive-links t)
  ;; Non-nil means C-a and C-e behave specially in headlines and items.
  (org-special-ctrl-a/e nil)
  ;; Non-nil mean font-lock should hide the emphasis "*, ~, /" marker characters.
  (org-hide-emphasis-markers t)
  ;; Non-nil means show entities as UTF8 characters.
  (org-pretty-entities t)
  ;; Insert Org headings at point, not after the current subtree
  (org-insert-heading-respect-content nil)

)