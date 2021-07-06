;******************************************
; ORG-ROAM ??q
;******************************************

;;; CODE:
(use-package org-roam
  :hook
    (after-init . org-roam-mode)
  :custom
    (org-roam-db-location "~/Org/zettelkasten/org-roam.db")
    (org-roam-directory "~/Org/zettelkasten")
    (benchmark 1 '(org-roam-db-build-cache))
    (org-roam-buffer-position 'right)
    (org-roam-buffer-width 0.3)
    (org-roam-index-file "index.org")
    (org-roam-verbose nil)  ; https://youtu.be/fn4jIlFwuLU
    (org-roam-buffer-no-delete-other-windows t) ; make org-roam buffer sticky
    (org-roam-title-change-hook '(org-roam--update-links-on-title-change))
    (org-roam-db-update-method 'immediate)
    (setq org-roam-dailies-directory "daily/")
    (setq org-roam-dailies-directory "~/Org/daily/")
    (org-roam-buffer-window-parameters '((no-delete-other-windows . t))
    (org-roam-completion-everywhere t)
    (org-roam-completion-system)
    (cond ((featurep! :completion helm) 'helm)
          ((featurep! :completion ivy) 'ivy)
          ((featurep! :completion ido) 'ido)
           ('default)))
    (defvar +org-roam-open-buffer-on-find-file t
      "If non-nil, open the org-roam buffer when opening an org roam file.")
;; Normally, the org-roam buffer doesn't open until you explicitly call
;; `org-roam'. If `+org-roam-open-buffer-on-find-file' is non-nil, the
;; org-roam buffer will be opened for you when you use `org-roam-find-file'
;; (but not `find-file', to limit the scope of this behavior).
   (add-hook 'find-file-hook
     (defun +org-roam-open-buffer-maybe-h ()
     (and +org-roam-open-buffer-on-find-file
       (memq 'org-roam-buffer--update-maybe post-command-hook)
       (not (window-parameter nil 'window-side)) ; don't proc for popups
       (not (eq 'visible (org-roam-buffer--visibility)))
       (with-current-buffer (window-buffer)
       (org-roam-buffer--get-create)))))


)

;;; Org-Roam-Templates
   (setq org-roam-capture-templates
  '(
   ("d" "default" plain (function org-roam--capture-get-point)
    "%?"
      :file-name
        "%(format-time-string \"%Y%m%d%H%M%S-${slug}\" (current-time) t)"
      :head

"#+TITLE: ${title}
#+AUTHOR: Sean Averhoff
#+CREATED: %U
#+LAST_MODIFIED: %U
#+STARTUP: OVERVIEW
#+ROAM_ALIAS:
#+ROAM_TAGS:
#+SETUPFILE: ~/Semacs/straight/repos/org-exsty/styles-html/retro_dark.theme \n"
      :unnarrowed t)

   ("b" "blog" plain (function org-roam--capture-get-point)
    "%?"
      :file-name  "%(format-time-string \"%Y%m%d%H%M%S-${slug}\" (current-time) t)"
      :head
"#+TITLE: ${title}
#+AUTHOR: Sean Averhoff
#+CREATED: %U
#+LAST_MODIFIED: %U
#+STARTUP: OVERVIEW
#+ROAM_ALIAS:
#+ROAM_TAGS:
#+SETUPFILE: ~/Semacs/straight/repos/org-exsty/styles-html/retro_dark.theme \n"
      :unnarrowed t)

  ("p" "private" plain (function org-roam--capture-get-point)
   "%?"
      :file-name
        "private-${slug}"
      :head
"#+TITLE: ${title}
#+Author: ${author-or-editor}
#+CREATED: %U
#+LAST_MODIFIED: %U
#+STARTUP: OVERVIEW"
      :unnarrowed t)))

(use-package company-org-roam
  :after org-roam
  :config
  (push 'company-org-roam company-backends))

;;** TODO Org-Roam-Bibtex
(use-package org-roam-bibtex
  :load-path "~/Org/zettelkasten" ;Modify with your own path
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
    (add-hook 'after-init-hook #'org-roam-bibtex-mode)
    (define-key org-roam-bibtex-mode-map (kbd "C-c n a") #'orb-note-actions)
)

(use-package org-roam-server
  :after org-roam
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-label-truncate t
        org-roam-server-label-truncate-length 60
        org-roam-server-label-wrap-length 20)
  (defun org-roam-server-open ()
    "Ensure the server is active, then open the roam graph."
    (interactive)
    (org-roam-server-mode 1)
    (browse-url-xdg-open (format "http://localhost:%d" org-roam-server-port))))


(use-package nroam
  :straight '(nroam :host github
                    :branch "master"
                    :repo "NicolasPetton/nroam")
  :config
  (add-hook 'org-mode-hook #'nroam-setup-maybe))

;;** Org-Roam-Block
;;Block-ref and block-embed are two concepts in Roam Research.
;;With the two functionalities, users can easily refer or include one
;;block in another block or page. Roam-block has implemented these in emacs.
;;[[https://github.com/Kinneyzhang/roam-block][GitHub - Roam-Block]]
;(use-package roam-block
 ; :after org-roam
  ;:load-path "path/to/roam-block/"
  ;  :hook (after-init . roam-block-mode)
 ;   :init (setq roam-block-home '("~/roam-block/")
 ;               roam-block-ref-highlight t
   ;             roam-block-embed-highlight t)
  ;  :bind
  ;  (:map roam-block-mode-map
     ;     (("C-c b r s" . roam-block-ref-store)
     ;      ("C-c b r i" . roam-block-ref-insert)
     ;      ("C-c b r d" . roam-block-ref-delete)
     ;      ("C-c b r t" . roam-block-ref-highlight-toggle)
     ;      ("C-c b e s" . roam-block-embed-store)
     ;      ("C-c b e i" . roam-block-embed-insert)
     ;      ("C-c b e t" . roam-block-embed-highlight-toggle)
     ;      ("C-c b d" . roam-block-delete-block))))
