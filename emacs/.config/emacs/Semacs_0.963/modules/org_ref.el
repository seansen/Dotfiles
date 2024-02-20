(use-package org-ref
  :after calibredb
  :config
  ;; org-ref
    (setq reftex-default-bibliography '("~/Calibre/catalog.bib"))

    (setq org-ref-bibliography-notes "~/Org/notes.org"
          org-ref-default-bibliography '("~/Calibre/catalog.bib")
          org-ref-pdf-directory "~/books/")
  ;; bibtex
    (setq bibtex-completion-bibliography "~/Calibre/catalog.bib"
          bibtex-completion-library-path "~/books"
          bibtex-completion-notes-path "~/Org/Helm-Bibtex-Notes")
;; KP
    (setq org-ref-completion-library 'org-ref-ivy-cite
          org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
          org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
          org-ref-notes-function 'orb-edit-notes)

(require 'org-ref)

;; calibredb
    (setq calibredb-ref-default-bibliography (concat (file-name-as-directory calibredb-root-dir) "catalog.bib"))
    ;(setq calibredb-ref-default-bibliography "~/Org/MeineBibliothek.bib")
    (add-to-list 'org-ref-default-bibliography calibredb-ref-default-bibliography)
    (setq org-ref-get-pdf-filename-function 'org-ref-get-mendeley-filename)

)
