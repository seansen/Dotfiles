;; Orderless
;; Use space-separated search terms in any order when completing with Icomplete or the default interface.

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))