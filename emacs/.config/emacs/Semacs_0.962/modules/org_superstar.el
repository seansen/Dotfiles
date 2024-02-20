(use-package org-superstar
  :config
    (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
    ;; Stop cycling bullets to emphasize hierarchy of headlines.
    (setq org-superstar-cycle-headline-bullets nil)
    ;; Hide away leading stars on terminal.
    (setq org-superstar-leading-fallback ?\s)
    (with-eval-after-load 'org-superstar
      (set-face-attribute 'org-superstar-item nil :height 1.)
      (set-face-attribute 'org-superstar-header-bullet nil :height 1.2)
      (set-face-attribute 'org-superstar-leading nil :height 1.3))
    ;; Set different bullets, with one getting a terminal fallback.
    (setq org-superstar-headline-bullets-list
      '(("\u262f" ?\u25c8) "\u25c9" "\u25cb" "\u25b7" "\u2738" "\u262f" "\u273f" "\u262f" "\u271c" "\u262f" "\u25c6" "\u262f" "\u25b6")))
