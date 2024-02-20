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
(load "enviroment-windows")                 ; Enviroment variables for windows
(load "enviroment-wsl")                     ; Enviroment variables for wsl
(load "enviroment-linux")                   ; Enviroment variables for linux

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
(load "appearance_nyan-mode")               ; Displays an animated cat in the mode-line.

;;; modules_file-management
(message "MODULES_FILE-MANAGEMENT")
(load "file-management_dired")              ; Manipulating files and directories within emacs.
(load "file-management_dired+")             ; Additional commands for files and directories.
(load "file-management_diredF1")            ; Documentation for the file or directory
(load "file-management_dired-icons")        ; Visually distinguish between different types of files
(load "file-management_dired-narrow")       ; A way to narrow the list of files and directories
(load "file-management_dired-open")         ; Open files with external programs
(load "file-management_dired-peep")         ; Quickly view file content
(load "file-management_dired-rainbow")      ; Colore coded file and directory names
(load "file-management_dired_sidebar")      ; quickly navigate and manipulate files.
(load "file-management_dired-subtree")      ; Displays content of a subtree.
(load "file-management_dired-hide-dotfiles"); Hide dotfiles

(load "file-management_semacs-file-viewer") ; Semacs -> Shortcut for personal files

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
(load "navigation_swiper")                  ; Overview of the current regex search candidates

;;; modules_auto-completion
(message "MODULES_AUTO-COMPLETION")
(load "auto-completion_corfu")              ; You can move point to any position in Emacs.
(load "auto-completion_cape")               ; You can move point to any position in Emacs.

;;; modules-editing
(message "MODULES_EDITING")
(load "editing_crux")                       ; A collection of useful interactive commands.
(load "editing_svg-tag-mode")               ; Provides syntax highlighting for SVG files.

;;; modules-search
(message "MODULES_SEARCH")
(load "search_affe")                        ; Asynchronous Fuzzy Finder for Emacs

;;; modules_utility
(message "MODULES_UTILITY")
(load "utility_undo-fu")                    ; User-friendly undo/redo system for users
(load "utility_undo-fu-session")            ; Simple, stable linear undo with redo for Emacs.
(load "utility_vundo")                      ; visual undo displays the undo history
(load "utility_undo-limit")                 ; Raising undo-limit to 80mb.
(load "utility_semacs-functions")           ; SplitWindowDiredToggle, Insert&CopyFileName, etc.
(load "editor_restart")                     ;(P) Restart Emcs.

;;; modules-help
(message "MODULES_HELP")
(load "help_helpful")                       ; Alternative to the built-in  help.

;;;_________
;;; modules-ui
(load "ui_info-color")                      ; Extra coloring package.
(load "ui_whitespace")                      ; Show spaces in open file.


;;; modules-editor
(load "editor_ws-butler")                   ; Trim spaces from end of line.
(load "editor_smartparens")                 ; Smartparens in specif modes
(load "editor_buffer-flip")                 ; Flip buffers inside of Emacs.
(load "editor_bufler")                      ; Sort Buffers.
(load "editor_move-line")                   ; Move line up and down.
(load "editor_quick-visit")                 ; Quickly visit config file.
(load "editor_rotate-windows")              ; Rotate windows.
(load "editor_neotree")                     ; Mode for quickly browsing, editing files.
(load "editor_open-with")                   ; Open files with external app.
;;(load "editor_helm")                      ; Framework for incremental completions and narrowing selections.
(load "editor_recentf")                     ; Save recently opened files.


;;; modules-store
(load "store_browse-kill-ring")             ; Interactively insert item from kill-ring.
(load "store_popup-kill-ring")              ; Interactively insert item from kill-ring.
(load "store_super-save")                   ; Super-Save saves buffers when they lose focus.
(load "store_remember-cursor")              ; Remember the cursor position.
(load "store_filename")                     ; Copy filename to clipboard or file.
(load "store_clipmon")                      ; Watches the system clipboard auto insert text.
(load "store_yasnippet")                    ; Snippets for coding and text files.
(load "store_ts")                           ; A date and time libary.
(load "store_insert-date")                  ; Insert current date.
;;(load "store_backup")                     ; Backup settings

;;; modules_shell-integration
(load "shell_pop-terminal")                         ;
;(load "shell_term")                        ; Terminal for Emacs.
;(load "shell_windows-shells")              ; Terminal Settings for Windows.

;;; modules-code
(load "code_projectile")                    ; Project folders.
(load "code_web-mode")                      ;
(load "programming_js")                     ; js-comint, js2-mode
(load "programming_haskel")                 ; js-comint, js2-mode
(load "programming_python")                 ; js-comint, js2-mode
(load "programming_php")                    ; js-comint, js2-mode
(load "compare_vdiff")                      ;
;(load "code_aggressive-indent-mode")        ; Minor mode that keeps your code always indented.

;;; modules-edit
(load "edit_markdown-to-org")               ;(F) Convert markdown buffer to org.
(load "edit_rename-file")                   ;(F) Rename current buffer-file
(load "edit_crux")                          ;(P) Convieniet functions.
(load "edit_pandoc")                        ;(P) A universal document converter.

;;; modules-research
(load "research_web-jump")                  ;(P)
(load "research_epub")                      ;(P) Nov is an Epub-reader.
(load "research_define-word")               ;(P)
(load "research_elfeed")                    ;(P)
(load "research_go-translate")              ;(P)
(load "research_google-translate")          ;(P)
(load "research_academic-phrases")          ;(P)
(load "research_calibre")

;;; modules-writing
(load "writing_amread-mode")                ;(P) This is a minor mode helping user speed-reading.
(load "writing_flycheck")                   ;(P)
(load "writing_auto-dictionary")            ;(P)
(load "writing_lorem-ipsum")                ;
;;;(load "writing_lsp-grammarly")           ;()
;;(load "phrasenschwein")

;;; modules-version-controll
(message "MODULES_VERSION-CONTROLL")
(load "version-controll_magit")
(load "version-controll_diff-hl")
(load "version-controll_git-timemachine")

;;; modules-extras
(message "MODULES_EXTRA")
(load "extras_weather")
(load "extras_speed-type")
;(load "ui_exwm")

;;; moudules_org-default
(message "MODULES_ORG_DEFAULT")
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

;;; modules_org-packages
(message "MODULES_ORG_PACKAGES")
(load "org_bars")                           ; Roma, Company, Nroam, Server,  Bibtex, Templates
(load "org_fancy-priorities")               ;
(load "org_treeusage")                      ;
(load "org_roam")                           ; Roma, Company, Nroam, Server, Bibtex, Templates
(load "org_contrib")                        ; Community packages for Org.
(load "org_functions")                      ;
(load "org_exsty")                          ; Org-Setup helpers.
;; (load "org_templates")                   ; Templates for Org-Mode.
;; (load "org_pdftools")                    ;
;; (load "org_export")                      ; Export Org-file to html.
;; (load "org_helm-org-rifle")              ; Searches rapidly through Org files.
;; (load "org_noter")                       ;
;; (load "org_code-blocks")                 ;
;; (load "org_ref")                         ;

;;; modules_depricated
(message "MODULES_DEPRICATED")
;(load "editor_ace-window")                 ; Switch windows inside of Emacs.
;(load "org_superstar")                     ;
;(load "ui_centaur-tabs")                   ;
;(load "ui_blackout")                       ; Hide major and minor modes in the mode line.
;(load "ui_rename-modeline")                ; Shorter names for modes in modeline.
;;(load "editor_ivy")                       ; A generic completion mechanism for Emacs.
;;(load "editor_deft")                      ; Find a file inside of a chosen folder.
;;(load "editor_company")                   ; Word completion
;(load "store_undo-tree")                   ; Allows you to recover  any past state of a buffer;.

(provide 'core-modules)

;;; core-modules.el ends here
