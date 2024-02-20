;;; core-modules.el --- Loads afer the init file -*- lexical-binding: t; -*-

;;; Commentary:
;;; - modules-environment
;;; - modules-appearance
;;; - modules-ui
;;; - modules-editor
;;; - modules-store
;;; - modules-terminal
;;; - modules-code
;;; - modules-edit
;;; - modules-ord
;;; - modules-research
;;; - modules-writing


;;; modules_environment
(message "MODULES_Enviroment")
(load "enviroment-windows")                 ;
(load "enviroment-wsl")                     ;
(load "enviroment-linux")                   ;

;;; modules_appearance
(message "MODULES_APPEARANCE")
(load "appearance_modeline")                ; Minions, Doom-Modeline
(load "appearance_themes")                  ; Doom-Themes, Circadia, S/Disable-Themes
(load "appearance_beacon")                  ; Flashes the cursor's line when you scroll.
(load "appearance_dashboard")               ; Startup Dashboard.
(load "appearance_emojify")                 ; Emojify support for Linux
(load "appearance_all-the-icons")           ; Icons for Emacs.
(load "appearance_rainbow-delimiters")      ; Rainbow delimiters.
(load "appearance_auto-dim-buffer")         ; Makes windows without focus less prominent.
(load "appearance_nyan-mode")               ; displays an animated cat in the mode-line.

;;; modules_file-management
(message "MODULES_FILE-MANAGEMENT")
(load "file-management_dired")              ; Dired
(load "file-management_file-viewer")        ; Semacs -> Shortcut for personal files

;;; modules_navigation                   
(message "MODULES_NAVIGATION")
(load "navigation_avy")                     ; You can move point to any position in Emacs.
(load "navigation_vertico")                 ; Vertico improves minibuffer navigation and completion in Emacs.
(load "navigation_embark")                  ; Context-based actions and previews in Emacs.
(load "navigation_embark-consult")          ; Enhanced previewing in Emacs.
(load "navigation_consult")                 ; Adds enhanced search and filtering to Emacs.
(load "navigation_marginal")                ; Displaying additional information and interacting.
(load "navigation_orderless")               ; Search completion for matching patterns.
(load "navigation_savehist")                ; Saves minibuffer & history commands between sessions

;;; modules-auto-completion
(message "MODULES-AUTO-COMPLETION")
(load "auto-completion_corfu")              ; You can move point to any position in Emacs.
(load "auto-completion_cape")               ; You can move point to any position in Emacs.

;;; modules-editing
(message "MODULES-EDITING")
(load "editing_crux")                       ; A collection of useful interactive commands.
(load "editing_svg-tag-mode")               ; Provides syntax highlighting for SVG files.

;;; modules_utility
(message "MODULES-UTILITY")
(load "utility_undo-fu")                    ; User-friendly undo/redo system for users
(load "utility_undo-fu-session")            ; Simple, stable linear undo with redo for Emacs.
(load "store_undo-limit")                   ;(C) Raising undo-limit to 80mb.
(load "store_undo-tree")                    ;(P) Allows you to recover  any past state of a buffer;.

;;; modules-ui
(load "ui_info-color")                      ;(P) Extra coloring package.
(load "ui_whitespace")                      ;(P) Show spaces in open file.
;(load "ui_blackout")                       ;(P) Hide major and minor modes in the mode line.
;(load "ui_rename-modeline")                ;!(F) Shorter names for modes in modeline.

;;; modules-help
(load "help_helpful")                         ;(P) https://github.com/Wilfred/helpful is an alternative to the built-in Emacs help that provides much more contextual information.

;;; modules-editor
(load "editor_ws-butler")                     ;(P) Trim spaces from end of line.
(load "editor_smartparens")                   ;(P) Smartparens in specif modes
;;(load "editor_ivy")                           ;(P) A generic completion mechanism for Emacs.
;;(load "editor_company")                       ;(P) Word completion

;;(load "editor_swiper")                      ;(P) Uses ivy to show an overview of all matches.
(load "editor_buffer-flip")                 ;(P) Flip buffers inside of Emacs.
(load "editor_bufler")                      ;(P) Sort Buffers.
(load "editor_move-line")                   ;(F) Move line up and down.
(load "editor_quick-visit")                 ;(F) Quickly visit config file.
;;(load "editor_deft")                        ;(P) Find a file inside of a chosen folder.
(load "editor_rotate-windows")              ;(F) Rotate windows.
(load "editor_neotree")                     ;(P) Mode for quickly browsing, editing files.
(load "editor_open-with")                   ;(P) Open files with external app.
(load "editor_restart")                       ;(P) Restart Emcs.
;;(load "editor_helm")                          ;(P) Framework for incremental completions and narrowing selections.
(load "editor_recentf")                       ;(P) Save recently opened files.

;;; modules-navigation
;;; modules-store

(load "store_browse-kill-ring")               ;(P) Interactively insert item from kill-ring.
(load "store_popup-kill-ring")                ;(P) Interactively insert item from kill-ring.
(load "store_super-save")                     ;(P) Super-Save saves buffers when they lose focus.
(load "store_remember-cursor")                ;(C) Remember the cursor position.
(load "store_filename")                       ;(P) Copy filename to clipboard or file.
(load "store_clipmon")                        ;(P) Watches the system clipboard auto insert text.
(load "store_yasnippet")                      ;(P) Snippets for coding and text files.
(load "store_ts")                             ;(P) A date and time libary.
(load "store_insert-date")                    ;(P) Insert current date.
;;(load "store_backup")                         ;(S)Backup settings

;;; modules_shell-integration

;(load "terminal_term")                        ;(I) Terminal for Emacs.
;(load "terinal_windows-shells")               ;(F) Terminal Settings for Windows.

;;; modules-code
(load "code_projectile")                      ;(P) Project folders.
(load "code_web-mode")                        ;(P)
(load "programming_js")                       ; js-comint, js2-mode
(load "programming_haskel")                       ; js-comint, js2-mode
(load "programming_python")                       ; js-comint, js2-mode
(load "programming_php")                       ; js-comint, js2-mode
;(load "code_aggressive-indent-mode")          ;(P) Minor mode that keeps your code always indented.


;;; modules-edit
(load "edit_markdown-to-org")                 ;(F) Convert markdown buffer to org.
(load "edit_rename-file")                     ;(F) Rename current buffer-file
(load "edit_crux")                            ;(P) Convieniet functions.
(load "edit_pandoc")                          ;(P) A universal document converter.

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
;;;(load "writing_lsp-grammarly")             ;()
;;(load "phrasenschwein")

;;; modules-version-controll
(load "version-controll_magit")
(load "version-controll_diff-hl")
(load "version-controll_git-timemachine")

;;; modules-search
;;(load "search_swiper")
(load "search_affe")

;;; modules-compare
(load "compare_vdiff")

;;; modules-extras
(load "extras_weather")
(load "extras_speed-type")
;(load "ui_exwm")
;

;;; moudules-org
(load "org_core")                               ; Core settings for Org-mode.
(load "org_faces")                              ; Visual styling to different elements.
(load "org_clock")                              ; Track the time spent on different tasks.
(load "org_blocks")                             ;
(load "org_todo")                               ;
(load "org_appear")                             ; Make invisible parts of Org elements appear visible.

(load "org_agenda")                             ;
(load "org_babel")                               ;
(load "org_export")                               ;
;(load "org_capture_doct")                  ; An alternative syntax for Org capture templates


;; (load "org_export")                          ;(P) Export Org-file to html.
;; (load "org_exsty")                           ;(P) Org-Setup helpers.
(load "org_fancy-priorities")                ;(P)
;; (load "org_helm-org-rifle")                  ;(P) Searches rapidly through Org files.
;; (load "org_noter")                           ;(P)
;; (load "org_code-blocks")                     ;(P)
;; (load "org_ref")                             ;(P)
(load "org_treeusage")                          ;(P)
;;; org-packages
(load "org_bars")                               ;(P) Roma, Company, Nroam, Server, Bibtex, Templates
(load "org_roam")                               ;(P) Roma, Company, Nroam, Server, Bibtex, Templates
(load "org_functions")                               ;(P)
;; (load "org_templates")                        ;(C) Templates for Org-Mode.
;; (load "org_pdftools")                         ;(C)

;;; modules-depricated
;(load "editor_ace-window")                     ;(P) Switch windows inside of Emacs.
;(load "org_superstar")                       ;
;(load "ui_centaur-tabs")

(provide 'core-modules)

;;; core-modules.el ends here
