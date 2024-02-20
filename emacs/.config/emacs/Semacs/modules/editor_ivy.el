;******************************************
; IVY
;******************************************

(use-package ivy
  :config
  (ivy-mode 1)
  ;;(setq search-default-mode #'char-fold-to-regexp)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-initial-inputs-alist nil)
  (setq enable-recursive-minibuffers t))

;Counsel is just ivy, but with a custom tailored UI for each specific situation.
(use-package counsel)

;Remember Commands that where uses
(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode 1)
  ;; Remember candidate frequencies across sessions
  (prescient-persist-mode 1)
  ;How many chosen candidates will be remembered
  (setq prescient-history-length 150)
  ;A multiplier applied to each frequent candidate each selection (default: 0.997)
  (setq prescient-frequency-decay 0.997)
  ;Threshold used for forgotten a command that isn't used frequently anymore (default: 0.5)
  (setq prescient-frequency-threshold 0.5)
  (setq prescient-filter-method '(literal regexp))
)

(use-package ivy-rich
  :after counsel
  :custom
  (ivy-virtual-abbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer)
  (ivy-rich-mode 1)) ;; this gets us descriptions in M-x.
