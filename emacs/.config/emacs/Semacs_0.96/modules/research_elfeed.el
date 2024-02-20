;;; PACKAGE --- STORE_ELFEED"
;;;******************************************
;;; COMMENTARY:
;;; Txxxxx
;;;******************************************


(use-package elfeed
  :config
    (setq elfeed-feeds'(
        "https://emacsformacosx.com/atom/daily"
        "https://www.reddit.com/r/emacs.rss"))
    ;(setq elfeed-use-curl nil)
    ;(add-to-list 'evil-emacs-state-modes 'elfeed-search-mode)
    ;(add-to-list 'evil-emacs-state-modes 'elfeed-show-mode)
     (setq elfeed-curl-program-name "/usr/bin/curl"))
