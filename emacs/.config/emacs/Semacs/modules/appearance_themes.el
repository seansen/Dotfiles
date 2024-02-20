(add-to-list 'custom-theme-load-path (concat user-emacs-directory
                                             "themes/"))
;; some themes have several variations (e.g. light and dark)
;; and share code between these variations in common elisp modules;
;; these modules need to be on the load path so that these themes work
(add-to-list 'load-path (concat user-emacs-directory
                                "themes/"))

; Disable-Themes

(defun semacs/disable-all-themes ()
  "Disable all active themes."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

;;Disable all themes before loading a theme
(defadvice load-theme (before disable-themes-first activate)
  (semacs/disable-all-themes))


; Doom-Themes
; https://github.com/hlissner/emacs-doom-themes
; An opinionated UI plugin and pack of themes extracted from my emacs.d, inspired by some of my 
; favorite color themes.

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t "If nil, bold is universally disabled")
  (doom-themes-enable-italic t "If nil, italics is universally disabled")
  (doom-themes-visual-bell-config "Enable flashing mode-line on errors")

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;(doom-themes-neotree-config)

  ;; or for treemacs users
  ;(setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  ;(doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

  ;(require 'doom-themes-ext-visual-bell)
  (doom-themes-visual-bell-config)

; Circadian
; https://github.com/GuidoSchmidt/circadian.el
; Adds the functionality to switch themes at night and day.

(use-package circadian
  :custom
  ;; (circadian-themes '((:sunrise . doom-gruvbox )
                      ;; (:sunset  . doom-gruvbox )))
  (circadian-themes '(("8:00" . doom-gruvbox )
                      ("19:00" . doom-gruvbox )))
  :config
  (circadian-setup))
