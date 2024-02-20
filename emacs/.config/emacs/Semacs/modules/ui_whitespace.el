;******************************************
; WHITESPACE
;******************************************
(use-package whitespace
  :defer t
)

;Delete trailing whitespace in all modes. Except when editing Markdown,
;because it uses two trailing blanks as a signal to create a line break.
(add-hook 'before-save-hook '(lambda()
                              (when (not (or (derived-mode-p 'markdown-mode)))
                                (delete-trailing-whitespace))))


(defun saa/flyspell-and-whitespace-mode ()
  "Toggle `flyspell-mode' and `whitespace-mode'."
  (interactive)
  (if (derived-mode-p 'prog-mode)
      (flyspell-prog-mode)
    (flyspell-mode)
    (when flyspell-mode
      (flyspell-buffer)))
  (whitespace-mode 'toggle))
