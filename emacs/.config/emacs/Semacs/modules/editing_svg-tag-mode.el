(use-package svg-tag-mode
  :straight (:repo "emacs-straight/svg-tag-mode"
                   :host github
                   :type git)
  :disabled
  :config
  (setq svg-tag-tags
      '(("\\(:[A-Z]+:\\)" . ((lambda (tag)
                               (svg-tag-make tag :beg 1 :end -1))))))

  (setq svg-tag-tags
      '(("\\(\"[A-Z]+\\)\-[a-zA-Z#0-9]+\"" . ((lambda (tag)
                                           (svg-tag-make tag :beg 1 :inverse t
                                                          :margin 0 :crop-right t))))
        ("\"[A-Z]+\\(\-[a-zA-Z#0-9]+\"\\)" . ((lambda (tag)
                                           (svg-tag-make tag :beg 1 :end -1
                                                         :margin 0 :crop-left t)))))))

