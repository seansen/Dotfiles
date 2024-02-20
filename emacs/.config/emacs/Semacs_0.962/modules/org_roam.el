;Org-Roam

(use-package org-roam
  :after org
  :preface
  ;; Immediat note insertion
  (defun org-roam-node-insert-immediate (arg &rest args)
    (interactive "P")
    (let ((args (cons arg args))
          (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                    '(:immediate-finish t)))))
      (apply #'org-roam-node-insert args)))


  :init
  (setq org-roam-directory "~/Org/Zettelkasten/")
  (setq org-id-locations-file "~/Org/Zettelkasten/.orgids")
  (setq org-roam-dailies-directory "~/Org/Zettelkasten/daily/")

  (add-to-list 'display-buffer-alist
                 '("\\*org-roam\\*"
                  (display-buffer-in-direction)
                  (direction . right)
                  (window-width . 0.33)
                  (window-height . fit-window-to-buffer)))

  (setq org-roam-v2-ack t)
  (require 'org-roam-protocol)

  :custom
  (org-roam-completion-everywhere t)
  (org-roam-complete-link-at-point t)

  ;; Minibuffer
  ;; https://org-roam.discourse.group/t/tags-not-showing-up-in-org-roam-find-list/2469
  ;; https://jethrokuan.github.io/org-roam-guide/
 ;; (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:40}" 'face 'org-tag)))
  :config
  (org-roam-db-autosync-enable)
  (org-roam-db-autosync-mode)
  (org-roam-setup)

  (setq org-roam-mode-section-functions
    (list #'org-roam-backlinks-section
          #'org-roam-reflinks-section
          #'org-roam-unlinked-references-section))



  (setq pop-up-windows nil)
  (add-hook 'org-roam-mode-hook
            (lambda ()
              (setq-local display-buffer--same-window-action
                          '(display-buffer-use-some-window
                            (main)))))

  ;; DAILYS TEMPLATE
  (setq org-roam-dailies-capture-templates
       '(("d" "default" entry
           "* %?"
          :if-new (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n"))))
)

;Org-Roam-Capture

(use-package org-roam
  :ensure nil
  :after org
  :config
  (setq org-roam-capture-templates
'(

;; DEFAULT TEMPLATE
("d" "default" plain "%?"
:if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
"#+title: ${title}
,#+author: Sean Averhoff
,#+created: %<%Y-%m-%d>
,#+startup: OVERVIEW\n")
:unnarrowed t)

;; BASIC TEMPLATE
("b" "Basiv" plain "%?"
:if-new (file+head "~/Org/Zettelkasten/${slug}.org"
"#+title: ${title}
,#+author: Sean Averhoff
,#+filetags: :agenda:
,#+created: %<%Y-%m-%d>
,#+archive: archive.org::* From %s
,#+startup: OVERVIEW\n")
:unnarrowed t)

;; CONTACT TEMPLATE
("c" "Contact" plain "%?"
:if-new (file+head "@${slug}.org"
"#+title: ${title}
,#+author: Sean Averhoff
,#+filetags: :person:
,#+created: %<%Y-%m-%d>
,#+startup: OVERVIEW\n")
:unnarrowed t)

;; ENCRYPTION TEMPLATE
("e" "Encryption" plain "%?"
:if-new (file+head "~/Org/Zettelkasten/${slug}.org.gpg"
"#+title: ${title}
,#+author: Sean Averhoff
,#+filetags: :permanent:
,#+created: %<%Y-%m-%d>
,#+startup: OVERVIEW\n")
:unnarrowed t)

;; LITERATUR TEMPLATE
("l" "Literatur Box" plain "* %?"
:if-new (file+head "~/Org/Zettelkasten/literatur/${citekey}.org"
"#+title: ${author}-${title}
,#+author:${author}
,#+filetags: :referencen:
,#+year:
,#+date: \n
\* Referenz
:PROPERTIES:
:NOTER_DOCUMENT: ${file}
:END:
")
:type org-roam-bibtex)

;; ARCHIVE TEMPLATE
("a" "Archive Box" plain "%?"
:if-new (file+head "~/Org/Zettelkasten/archive/${slug}.org"
"#+title: ${title}
,#+author: Sean Averhoff
,#+filetags: :archive:
,#+created: %<%Y-%m-%d>
,#+startup: OVERVIEW\n")
:unnarrowed t)
)))


;; Org-Roam-UI

(use-package org-roam-ui
  :straight (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :hook (after-init . org-roam-ui-mode)
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))


;; Org-Roam-Transclusion

(use-package org-transclusion
  :after org-roam
  :config
  (setq org-transclusion-mode t)
  (org-transclusion-mode t)
  (add-to-list 'org-transclusion-extensions 'org-transclusion-indent-mode)
  )
  ;; (custom-set-faces
  ;;  '(org-transclusion-fringe
  ;;  ((t (:background "#2a2a2ai" :weight bold :foreground "white"))))))


;; Org-Roam-Consult

(use-package consult-org-roam
   :straight (:host github :repo "jgru/consult-org-roam")
   :disabled
   :custom
   (consult-org-roam-grep-func #'consult-ripgrep)
   ;; Eventually suppress previewing for certain functions
   :config
   (consult-org-roam-mode 1))

   (consult-customize
   :preview-key (kbd "M-."))


;; Org-Roam-Lister
;;[[https://github.com/publicimageltd/lister][Lister]]

(use-package lister
  :straight (:repo "publicimageltd/lister" :host github :type git))


;; Org-Roam-Delve
;;[[https://github.com/publicimageltd/delve][Delve]] is a package on top of Org Roam. It provides tools to collect, inspect and edit Org Roam Nodes in a separate application buffer.

(use-package delve
  :straight (:repo "publicimageltd/delve":host github :type git)
  :bind
  (("C-<f12>" . delve))
  :config
  (setq delve-dashboard-tags '("person" "coding" "js"))
  (delve-global-minor-mode))

  (defun delve-sort-buffer-function (buf function)
    "sort all items in BUF by FUNCTION."
    (when-let* ((all-data (lister-get-all-data buf))
                (head (car all-data))
                (tail (cdr all-data)))
                (lister-with-locked-cursor buf
                  (with-temp-message "Updating the whole buffer, that might take some time...."
                   (lister-set-list buf (cons head (funcall function  tail)))))))
