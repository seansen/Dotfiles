(use-package org
  :ensure nil
  :custom
  ;; Don't ask before executing
  (org-confirm-babel-evaluate 'nil)
  (org-src-fontify-natively t)

  ;; Edit in current window
  (org-src-window-setup 'current-window)
  (org-src-strip-leading-and-trailing-blank-lines t)

  ;; Do not put two spaces on the left
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)


  (org-structure-template-alist
   '(("s" . "src")
     ("el" . "src emacs-lisp")
     ("es" ("src emacs-lisp :results silent")
      ("e" . "example")
      ("q" . "quote")
      ("t" . "quote")
      ("v" . "verse")
      ("V" . "verbatim")
      ("c" . "center")
      ("C" . "comment"))))

  ;; Never evaluate code blocks upon export and replace results when evaluation does occur.
  ;; For a particular language 𝑳, alter ‘org-babel-default-header-args:𝑳’.
  ;; (org-babel-default-header-args
  ;;  '((:results . "replace")
  ;;    (:session . "none")
  ;;    (:exports . "both")
  ;;    (:cache .   "no")
  ;;    (:noweb . "no")
  ;;    (:hlines . "no")
  ;;    (:tangle . "no")
  ;;    (:eval . "never-export")))

  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (plantuml . t)
     (dot . t)
     (js . t)
                                        ;(php . t)
     (emacs-lisp . t)
     (python . t)
     (latex . t)
     (shell . t))
   )
  )
