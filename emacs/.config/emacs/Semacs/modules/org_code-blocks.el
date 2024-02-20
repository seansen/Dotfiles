
(setq org-confirm-babel-evaluate 'nil)             ; Don't ask before executing

;(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
;(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
;(add-to-list 'org-structure-template-alist '("py" . "src python"))
;(add-to-list 'org-structure-template-alist '("h" . "src php"))
;(add-to-list 'org-structure-template-alist '("js" . "src js"))


;;; Setup code block templates.
;; For Org-mode < 9.2
;; (setq old-structure-template-alist
;;       '(("py" "#+BEGIN_SRC python :results output\n?\n#+END_SRC" "")
;;         ("ipy" "#+BEGIN_SRC ipython :results output\n?\n#+END_SRC" "")
;;         ("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC" "")
;;         ("hs" "#+BEGIN_SRC haskell\n?\n#+END_SRC" "")
;;         ("sh" "#+BEGIN_SRC sh\n?\n#+END_SRC" "")
;;         ("r" "#+BEGIN_SRC R\n?\n#+END_SRC" "")
;;         ("js" "#+BEGIN_SRC js\n?\n#+END_SRC" "")
;;         ("http" "#+BEGIN_SRC http\n?\n#+END_SRC" "")
;;         ("ditaa" "#+BEGIN_SRC ditaa :file\n?\n#+END_SRC" "")
;;         ("dot" "#+BEGIN_SRC dot :file\n?\n#+END_SRC" "")
;;         ("rp" "#+BEGIN_SRC R :results output graphics :file \n?\n#+END_SRC" "")
;;         ("plantuml" "#+BEGIN_SRC plantuml :file\n?\n#+END_SRC" "")
;;         ("n" "#+NAME: ?")
;;         ("cap" "#+CAPTION: ?")))

;;   ;; For Org-mode >= 9.2
;;   (setq new-structure-template-alist
;;       '(("py" . "src python :results output")
;;         ("el" . "src emacs-lisp")
;;         ("sh" . "src sh")
;;         ("js" . "src js")
;;         ("php" . "src php")
;;         ("ditaa" . "src ditaa :file")
;;         ("dot" . "src dot :file")
;;         ))

;;   ;; Keyword expansion also changed in 9.2
;;   (setq my-tempo-keywords-alist
;;         '(("n" . "NAME")
;;           ("cap" . "CAPTION")))

;;   (when (version< (org-version) "9.2")
;;     (add-to-list 'org-modules 'org-tempo))
;;   (require 'org-tempo)
;;   (if (version<  (org-version) "9.2")
;;       (dolist (ele old-structure-template-alist)
;;         (add-to-list 'org-structure-template-alist ele))
;;     (dolist (ele new-structure-template-alist)
;;       (add-to-list 'org-structure-template-alist ele))
;;     (dolist (ele my-tempo-keywords-alist)
;;       (add-to-list 'org-tempo-keywords-alist ele))
;;     )

(org-babel-do-load-languages
'org-babel-load-languages
'(
 (R . t)
   (dot . t)
   (js . t)
   ;(php . t)
   (emacs-lisp . t)
   (python . t)
   (latex . t)
   (shell . t)
  ))

;Org-Babel-Block-Setting
(setq org-src-fontify-natively t
      org-src-window-setup 'current-window         ;edit in current window
      org-src-strip-leading-and-trailing-blank-lines t
      org-src-preserve-indentation t               ; do not put two spaces on the left
      org-src-tab-acts-natively t
      org-src-preserve-indentation t)

;Org-Rich-Yank
;Yank source code into your org files, manually surrounding it in #+BEGIN_SRC blocks
(use-package org-rich-yank
  :defer 6)
