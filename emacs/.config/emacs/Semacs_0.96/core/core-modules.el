;;; core-modules.el --- Loads afer the init file -*- lexical-binding: t; -*-

;;; Commentary:
;;; - modules-ui
;;; - modules-editor
;;; - modules-store
;;; - modules-terminal
;;; - modules-code
;;; - modules-edit
;;; - modules-ord
;;; - modules-research
;;; - modules-writing

;;; Code:
;;(error "Stopping .emacs")


;;; modules-ui
(load "ui_all-the-icons")                     ;(P) Icons for Emacs.
(load "ui_auto-dim-buffer")                   ;(P) Auto dim buffers.
(load "ui_dashboard")                         ;(P) Startup Dashboard.
(load "ui_info-color")                        ;(P) Extra coloring package.
(load "ui_rainbow-delimiters")                ;(P) Rainbow delimiters.
(load "ui_themes")                            ;(PM) Remember-Last-Theme and Doom-Themes.
(load "ui_whitespace")                        ;(P) Show spaces in open file.
(load "ui_doom-modeline")                     ;(P) Doom-Modeline for vanilla Emacs.
(load "ui_blackout")                          ;(P) Hide major and minor modes in the mode line.
(load "ui_nyan-mode")                         ;(P) Shows buffer position in mode-line with a cat.
;(load "ui_rename-modeline")                   ;!(F) Shorter names for modes in modeline.
(load "ui_centaur-tabs")

;;; modules-help
(load "help_helpful")                         ;(P) https://github.com/Wilfred/helpful is an alternative to the built-in Emacs help that provides much more contextual information.

;;; modules-editor
(load "editor_ws-butler")                     ;(P) Trim spaces from end of line.
(load "editor_smartparens")                   ;(P) Smartparens in specif modes
(load "editor_ivy")                           ;(P) A generic completion mechanism for Emacs.
(load "editor_company")                       ;(P) Word completion
(load "editor_avy")                         ;(P) You can move point to any position in Emacs.
(load "editor_swiper")                      ;(P) Uses ivy to show an overview of all matches.
(load "editor_ace-window")                  ;(P) Switch windows inside of Emacs.
(load "editor_buffer-flip")                 ;(P) Flip buffers inside of Emacs.
(load "editor_bufler")                      ;(P) Sort Buffers.
(load "editor_move-line")                   ;(F) Move line up and down.
(load "editor_quick-visit")                 ;(F) Quickly visit config file.
(load "editor_deft")                        ;(P) Find a file inside of a chosen folder.
(load "editor_rotate-windows")              ;(F) Rotate windows.
(load "editor_neotree")                     ;(P) Mode for quickly browsing, editing files.
(load "editor_open-with")                   ;(P) Open files with external app.
(load "editor_dired")                         ;(P) Browse files.
(load "editor_restart")                       ;(P) Restart Emcs.
(load "editor_helm")                          ;(P) Framework for incremental completions and narrowing selections.
(load "editor_recentf")                       ;(P) Save recently opened files.
;;; modules-store

(load "store_browse-kill-ring")               ;(P) Interactively insert item from kill-ring.
(load "store_popup-kill-ring")                ;(P) Interactively insert item from kill-ring.
(load "store_super-save")                     ;(P) Super-Save saves buffers when they lose focus.
(load "store_remember-cursor")                ;(C) Remember the cursor position.
(load "store_undo-tree")                      ;(P) Allows you to recover  any past state of a buffer;.
(load "store_undo-fu")                        ;(P) Simple, stable linear undo with redo for Emacs.
(load "store_undo-limit")                     ;(C) Raising undo-limit to 80mb.
(load "store_filename")                       ;(P) Copy filename to clipboard or file.
(load "store_clipmon")                        ;(P) Watches the system clipboard auto insert text.
(load "store_yasnippet")                      ;(P) Snippets for coding and text files.
(load "store_ts")                             ;(P) A date and time libary.
(load "store_insert-date")                    ;(P) Insert current date.
;;(load "store_backup")                         ;(S)Backup settings

;;; modules-terminal
(load "terminal_term")                        ;(I) Terminal for Emacs.
;(load "terinal_windows-shells")               ;(F) Terminal Settings for Windows.

;;; modules-code
(load "code_projectile")                      ;(P) Project folders.
(load "code_web-mode")                        ;(P)
(load "code_php-mode")                        ;(P)
(load "code_python")                          ;(P)
(load "code_js-comint")                       ;(P) Run a JavaScript interpreter in an inferior process window.
(load "code_js2-mode")
;(load "code_aggressive-indent-mode")          ;(P) Minor mode that keeps your code always indented.

;;; modules-edit
(load "edit_markdown-to-org")                 ;(F) Convert markdown buffer to org.
(load "edit_rename-file")                     ;(F) Rename current buffer-file
(load "edit_crux")                            ;(P) Convieniet functions.
(load "edit_pandoc")                          ;(P) A universal document converter.

;;; moudules-org
(load "org_core")                                ;(P) Core settings for Org-mode.
(load "org_faces")                               ;(P) 
(load "org_clock")                               ;(P) 
(load "org_blocks")                               ;(P) 
;; (load "org_superstar")                        ;?(P)
;; (load "org_appear")                           ;(P)
;; (load "org_export")                           ;(P) Export Org-file to html.
;; (load "org_exsty")                            ;(P) Org-Setup helpers.
;; (load "org_fancy-priorities")                 ;(P)
;; (load "org_helm-org-rifle")                   ;(P) Searches rapidly through Org files.
;; (load "org_noter")                            ;(P)
;; (load "org_code-blocks")                      ;(P)
;; (load "org_ref")                              ;(P)

;;; org-packages
(load "org_bars")                             ;(P) Roma, Company, Nroam, Server, Bibtex, Templates
(load "org_roam")                             ;(P) Roma, Company, Nroam, Server, Bibtex, Templates
;; (load "org_templates")                        ;(C) Templates for Org-Mode.
;; (load "org_pdftools")                         ;(C)
(load "org_functions")                               ;(P) 

;;; modules-research
(load "research_web-jump")                    ;(P)
(load "research_epub")                        ;(P) Nov is an Epub-reader.
(load "research_define-word")                 ;(P)
(load "research_elfeed")                      ;(P)
(load "research_go-translate")                ;(P)
(load "research_google-translate")            ;(P)
(load "research_academic-phrases")            ;(P)
(load "research_calibre")

;;; modules-writing
(load "writing_amread-mode")                  ;(P) This is a minor mode helping user speed-reading.
(load "writing_flycheck")                     ;(P)
(load "writing_auto-dictionary")              ;(P)
(load "writing_lorem-ipsum")
;;;(load "writing_lsp-grammarly")                ;()
;;(load "phrasenschwein")

;;; modules-git
(load "git_magit")

;;; modules-extras
(load "extras_weather")
(load "extras_speed-type")
;(load "ui_exwm")
;
(provide 'core-modules)

;;; core-modules.el ends here
