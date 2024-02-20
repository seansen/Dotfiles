;;; Flycheck
(use-package flycheck
  :after org
  :hook
  (org-src-mode . disable-flycheck-for-elisp)
  :custom
  (flycheck-emacs-lisp-initialize-packages t)
  (flycheck-display-errors-delay 0.1)
  :config
  (global-flycheck-mode)
  (flycheck-set-indication-mode 'left-margin)

  (defun disable-flycheck-for-elisp ()
    (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

  (add-to-list 'flycheck-checkers 'proselint)
  (setq-default flycheck-disabled-checkers '(haskell-stack-ghc)))

;;; Flycheck-Inline
(use-package flycheck-inline
  :config (global-flycheck-inline-mode))

;;; Flyspell-Correct
(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))


;;;???????????
;  :init
 ; (global-flycheck-mode t))

  ;(add-hook 'text-mode-hook 'flyspell-mode)
  ;(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;;; Flyspell with Whitespace
(defun saa/flyspell-and-whitespace-mode ()
  "Toggle `flyspell-mode' and `whitespace-mode'."
  (interactive)
  (if (derived-mode-p 'prog-mode)
      (flyspell-prog-mode)
    (flyspell-mode)
    (when flyspell-mode
      (flyspell-buffer)))
  (whitespace-mode 'toggle))

(global-set-key (kbd "<f9>") #'saa/flyspell-and-whitespace-mode)

;;; LSP-GRAMMARLY
(straight-use-package
 '(lsp-grammarly :type git :host github :repo "emacs-grammarly/lsp-grammarly"
  :hook (text-mode . (lambda ()
                       (require 'lsp-grammarly)
                       (lsp)))))  ; or lsp-deferred
