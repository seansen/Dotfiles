(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "js2-mode" js2-mode "JS2")
(rename-modeline "clojure-mode" clojure-mode "Clj")
(rename-modeline "Org-roam" org-roam-mode "OR")
(rename-modeline "Emacs-Lisp" org-roam-mode "EL")
(rename-modeline "FancyPriorities" org-fancy-priorities-mode "FP")
(rename-modeline "super-save" super-save-mode "FP")
