(use-package pandoc-mode)

(use-package ox-pandoc
  :after org)
;; default options for all output formats
;(setq org-pandoc-options '((standalone . _)))
;; cancel above settings only for 'docx' format
;(setq org-pandoc-options-for-docx '((standalone . nil)))
;; special settings for beamer-pdf and latex-pdf exporters
;(setq org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex")))
;(setq org-pandoc-options-for-latex-pdf '((pdf-engine . "pdflatex")))
;; special extensions for markdown_github output
;(setq org-pandoc-format-extensions '(markdown_github+pipe_tables+raw_html))

;; open docx files in default application (ie msword)
(setq org-file-apps
      '(("\\.docx\\'" . default)
        ("\\.mm\\'" . default)
        ("\\.x?html?\\'" . default)
        ("\\.pdf\\'" . default)
        (auto-mode . emacs)))
