;;; core-keybinds.el --- Loads afer the early-init file -*- lexical-binding: t; -*-

;;; core.el --- Loads afer the init file -*- lexical-binding: t; -*-

;;; Commentary:
;;; - Evil
;;; - Which-key
;;; - General


;;; Evil
;;; https://evil.readthedocs.io/en/latest/settings.html#the-initial-state
(use-package evil
  :init
  (setq visual-line-mode t)

  :custom
  (evil-move-cursor-back nil)
  (evil-move-beyond-eol t)
  (evil-track-eol nil)
  (evil-undo-system 'undo-fu)
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-want-C-i-jump nil)

  :config
  ;; https://wikemacs.org/index.php/Evil#Enter_an_emacs_mode_in_a_given_state
  (evil-set-initial-state 'git-commit-mode 'insert)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-mode 1)

  ;; Normal state
  (define-key evil-normal-state-map "ü" 'consult-yank-pop)
  (define-key evil-normal-state-map "U" 'vundo)

  ;; Define evil-ex-map
  (define-key evil-ex-map "bb" 'helm-buffers-list)
  (define-key evil-ex-map "bk" 'kill-current-buffer)
  (define-key evil-ex-map "bK" 'kill-buffer)
  (define-key evil-ex-map "bv" 'evil-window-vsplit)
  (define-key evil-ex-map "bh" 'evil-window-split)
  (define-key evil-ex-map "br" 'ssa/rotate-windows)
  (define-key evil-ex-map "qr" 'restart-emacs)
  (define-key evil-ex-map "qn" 'restart-emacs-start-new-emacs)

  ;; Evil Force Quit
  (define-key evil-normal-state-map "\C-g" 'evil-force-normal-state)
  (define-key evil-visual-state-map "\C-g" 'evil-exit-visual-state)
  (define-key evil-insert-state-map "\C-g" 'evil-normal-state)
  (define-key evil-replace-state-map "\C-g" 'evil-normal-state)
  (define-key evil-ex-completion-map "\C-g" 'abort-recursive-edit)

  ;; Define Visual line motions outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; Text scaling
  (global-set-key (kbd "C-+") 'text-scale-increase)
  (global-set-key (kbd "C-*") 'text-scale-set)
  (global-set-key (kbd "C--") 'text-scale-decrease)
  (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
  (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)

  (global-set-key (kbd "C-`") 'org-cycle-agenda-files)
  (global-set-key (kbd "C-ß") 'semacs/org-cycle-files)

  (setq evil-emacs-state-cursor '("goldenrod4" bar))
  (setq evil-normal-state-cursor '("green4" box))
  (setq evil-visual-state-cursor '("royal blue" box))
  (setq evil-insert-state-cursor '("red3" bar))
  (setq evil-replace-state-cursor '("yellow" bar))
  (setq evil-test-state-cursor '("black" bar))
  (setq evil-operator-state-cursor '("blue" hollow))

)

;;; Evil-Test-State
(evil-define-state test
  "Test state."
  :tag " <T> "
  (message (if (evil-test-state-p)
               "Enabling test state."
             "Disabling test state.")
           ))

;;; Evil-Escape
;;; Customizable key sequence to escape from insert state and everything else in Emacs.

(use-package evil-escape
  :after evil
  :preface
  (defvar semacs-escape-hook nil
    "A hook run when C-g is pressed (or ESC in normal mode, for evil users).
     More specifically, when `doom/escape' is pressed. If any hook returns non-nil,
     all hooks after it are ignored.")

  (defun semacs/escape (&optional interactive)
    "Run `semacs-escape-hook'."
    (interactive (list 'interactive))
    (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (when interactive
           (setq this-command 'abort-recursive-edit))
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((run-hook-with-args-until-success 'doom-escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((unwind-protect (keyboard-quit)
           (when interactive
             (setq this-command 'keyboard-quit))))))

  :config
  (evil-escape-mode t)

  :custom
  (evil-move-cursor-back nil)
  (evil-escape-key-sequence "jk")
  (evil-escape-delay 0.63)
  (evil-escape-unordered-key-sequence t))

;;; Evil-Easymotion
;;; With evil-easymotion you can invoke SPCj, and this plugin will put a target character on every possible position.
;;; Keybindings: (ö-j /ö-k)

(use-package evil-easymotion
  :after evil
  :config
  (evilem-default-keybindings "ö"))


;;; Evil-Commentary
;;; Intends to make it easy to comment out (lines of) code.
;;; Keybindings: (g-c-c)

(use-package evil-nerd-commenter
  :straight (:repo "redguardtoo/evil-nerd-commenter" :host github :type git))
  ;; :custom
  ;; (evil-leader/set-key
  ;;   "ci" 'evilnc-comment-or-uncomment-lines
  ;;   "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
  ;;   "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
  ;;   "cc" 'evilnc-copy-and-comment-lines
  ;;   "cp" 'evilnc-comment-or-uncomment-paragraphs
  ;;   "cr" 'comment-or-uncomment-region
  ;;   "cv" 'evilnc-toggle-invert-comment-line-by-line
  ;;   "."  'evilnc-copy-and-comment-operator
  ;;   ; if you prefer backslash key
  ;;   "\\" 'evilnc-comment-operator))


;;; Evil-Goggles
;;; [[https://github.com/edkolev/evil-goggles][Evil-Goggles]] displays a visual hint when editing with evil.

(use-package evil-goggles
  :config
  (evil-goggles-mode)

  :custom
  (evil-goggles-pulse t)
  (evil-goggles-duration 0.200)
  (evil-goggles-async-duration 0.400)

 :custom-face
 (evil-goggles-default-face ((t (:foreground "blue" :background "green"))))
 (evil-goggles-delete-face ((t (:foreground "blue" :background "green"))))
 (evil-goggles-paste-face ((t (:foreground "blue" :background "green"))))
 (evil-goggles-yank-face ((t (:foreground "blue" :background "green")))))


;;; Evil-Collection
;;; [[https://github.com/emacs-evil/evil-collection][Evil-Collection]] is a collection of Evil bindings for the parts of Emacs that Evil does not cover properly by default, such as help-mode, M-x calendar, Eshell and mor

(use-package evil-collection
   :after evil
   :custom
   (evil-collection-setup-minibuffer t)
   (evil-collection-calendar-want-org-bindings t)
   (evil-want-keybinding nil)
   :config
   ;(setq evil-collection-mode-list nil)
   (evil-collection-init))

;;; Org-Evil
;;; Provides integration between Evil and Org through various means such as contextual keybindings.
(use-package evil-org
  :after org-evil
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  :custom
  (evil-org-agenda-set-keys t))

;;; Evil-Org
;;; Provides integration between Evil and Org through various means such as contextual keybindings.
(use-package org-evil
  :after evil)

;;; Evil-Exchange
;;; [[https://github.com/Dewdrops/evil-exchange][Evil-Exchange]] is an easy text exchange operator for Evil

(use-package evil-exchange
  :after evil
  :custom
  (evil-exchange-cx-install)
  (evil-exchange-install))

;;; Evil-Mutiedit
;;; M-d M-D
(use-package evil-multiedit
  :straight (:repo "hlissner/evil-multiedit" :host github :type git)
  :config

  ;; Highlights all matches of the selection in the buffer.
  (define-key evil-visual-state-map "R" 'evil-multiedit-match-all)

  ;; Match the word under cursor (i.e. make it an edit region). Consecutive presses will
  ;; incrementally add the next unmatched match.
  (define-key evil-normal-state-map (kbd "M-d") 'evil-multiedit-match-and-next)
  ;; Match selected region.
  (define-key evil-visual-state-map (kbd "M-d") 'evil-multiedit-match-and-next)
  ;; Insert marker at point
  (define-key evil-insert-state-map (kbd "M-d") 'evil-multiedit-toggle-marker-here)

  ;; Same as M-d but in reverse.
  (define-key evil-normal-state-map (kbd "M-D") 'evil-multiedit-match-and-prev)
  (define-key evil-visual-state-map (kbd "M-D") 'evil-multiedit-match-and-prev)    )

;;; Not-Evil
;;; There are many modes that are not centered about text-manipulation.

(mapc (lambda (mode)
  (evil-set-initial-state mode 'emacs))
    '(;;elfeed-show-mode
      ;;elfeed-search-mode
      calibredb-search-mode
      git-timemachine-mode
      calibredb-show-mode
      eshell-mode
      ; which-key-mode
      delve-global-minor-mode
      delve-mode
      forge-pullreq-list-mode
      helm-mode
      forge-topic-list-mode
      org-noter
      dired-mode
      tide-references-mode
      image-dired-mode
      bufler-mode
      image-dired-thumbnail-mode
      eww-mode))


;;; Unbind

(global-set-key (kbd "C-x k") nil)
(global-set-key (kbd "C-<SPC>") nil)
(global-set-key (kbd "C-<SPC>") nil)
(define-key evil-normal-state-map (kbd "q") nil)
;(define-key treemacs-mode-map (kbd "<prior>") nil)
;(define-key treemacs-mode-map (kbd "<next>") nil)
;(define-key treemacs-mode-map (kbd "M-P") nil)

;(define-key org-mode-map (kbd "?\C-\t") 'unpackaged/org-agenda-toggle-preview)
;(define-key org-agenda-mode-map (kbd "K") 'unpackaged/org-agenda-toggle-preview)

;;; General
(use-package general
  :config
  (general-evil-setup t)
  (general-define-key
    "M-<shift>-<up>"   'drag-line-up                              ;
    "M-<shift>-<down>" 'drag-line-down)

  (general-define-key
      "<escape>"       'keyboard-escape-quit                      ; Make ESC quit prompts
      "M-o"            'org-open-at-point                         ; Open link in org-mode
      "M-ö"            'consult-org-heading                       ; Replace default M-x with ivy backend
  )

  (general-define-key

      "M-#"            'evilnc-comment-or-uncomment-lines         ;
      "M-ü"            'evil-exchange                             ;
      "<shift>-M-Ü"    'evil-exchange                             ;
      ;"M-p"            'other-window                              ;
      ;"C-ä"            'which-key-show-next-page-cycle
      "C-M-ü"          'evil-exchange-cancel
      "C-s"            'swiper                                    ; Search for string in buffer
      "C-x k"          'kill-this-buffer
      "M-p"            'dired-sidebar-toggle-sidebar
      "M-P"            'semacs/ol-org-tree-toggle-kill
      "C-K"            'kill-dired-buffers                        ;
      "<f5>"           'dired-sidebar-toggle-sidebar
      "<f7>"           'which-key-show-major-mode
      "<f8>"           'org-transclusion-add
      "C-<f8>"         'clipmon-autoinsert-toggle                 ;
      "C-<f12>"          'delve
      "<f9>"           'open-calendar;
      ;"<f6>"           'centaur-tabs-counsel-switch-group         ;CENTAUR_TAB

      ;"C-<prior>"      'centaur-tabs-backward                     ;
      ;"C-<next>"       'centaur-tabs-forward                      ;
      "C-<prior>"      'previous-buffer                           ;
      "C-<next>"       'next-buffer                               ;
      ;"<f10>"         'other-window                              ;
      ;"C-<prior>"     'awesome-tab-backward-tab                  ;
      ;"C-<next>"      'awesome-tab-forward-tab                   ;
      "<prior>"        'previous-window-any-frame
      "<next>"         'next-window-any-frame)                    ;
)

;;; My-Leader
(general-create-definer my-leader-def
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

(my-leader-def
 :states 'normal
  "SPC" '(:ignore t :which-key "LEDDER")
  "SPC SPC" #'keyboard-escape-quit
  "SPC c" '(centered-cursor-mode :which-key "Centered Cursor Mode")
  "SPC m" '(bookmark-set :which-key "Bookmark Set")
  "SPC d" '(bookmark-delet :which-key "Boomark-Delete")
  "SPC j" '(bookmark-jump :which-key "Bookmark Jump")
  "SPC l" '(bookmark-bmenu-list :which-key "Bookmark List")
  "SPC t" '(bm-toggle :which-key "BM-Toggle")
  "SPC p" '(bm-next :which-key "BM-Next")
  "SPC n" '(bm-previous :which-key "BM-Previous")
  "SPC w" '(wttrin :which-key "Get Weather")
  "SPC e" '(:ignore t :which-key "EXPORT")
  "SPC e e" '(org-html-export-to-html      :which-key "Export HTML")
  "SPC e l" '(org-latex-export-to-pdf :which-key "Export Latex-PDF")
  "SPC e d" '(org-pandoc-export-to-docx    :which-key "Export DOCX")
  "SPC e d" '(org-pandoc-export-to-ms-pdf  :which-key "Export PDF")
  "SPC e m" '(org-pandoc-export-to-gfm-and-open  :which-key "Org to Markdown")
  "SPC e o" '(saa/markdown-convert-buffer-to-org :which-key "Markdown to Org")
)

;;; Alarm
(my-leader-def
  :states 'normal
    "a" '(:ignore t                     :which-key "ALARM")
    "a i" '(org-clock-in                :which-key "Clock in")
    "a o" '(org-clock-out               :which-key "Clock out")
    "a t" '(org-clock-report            :which-key "Clock table")
    "a d" '(org-clock-display           :which-key "Display clock")
    "a l" '(org-clock-in-last           :which-key "Clock last")
    "a a" '(org-timer-set-timer         :which-key "Set timer")
    "a s" '(org-timer-start             :which-key "Start timer")
    "a S" '(org-timer-stop              :which-key "Stop timer")
    "a p" '(org-timer-pause-or-continue :which-key "Continue timer")
    "a c" '(org-timer-pause-or-continue :which-key "Continue timer"))


;;; Buffer
(my-leader-def
  :states 'normal
    "b" '(:ignore t                     :which-key "BUFFER")
    "b b" '(consult-buffer              :which-key "List all buffers")
    "b t" '(bm-toggle                   :which-key "Bookmark-Toggle")
    "b n" '(bm-next                     :which-key "Bookmark Next")
    "b p" '(bm-previous                 :which-key "Bookmark Previous")
    "b k" '(kill-current-buffer         :which-key "Kill Buffer")
    "b K" '(crux-kill-other-buffers     :which-key "Kill all buffers")
    "b d" '(kill-dired-buffers          :which-key "Kill Dired buffers")
    "b D" '(kill-all-dired-buffers      :which-key "Kill all Direed buffers")
    "b v" '(evil-window-vsplit          :which-key "Split Window vertical")
    "b V" '(semacs/window-split         :which-key "Split Window vertical and open Dired")
    "b h" '(evil-window-split           :which-key "Split Window horizontaly")
    "b c" '(vdiff-buffers               :which-key "Compare Files")
    "b r" '(crux-swap-windows           :which-key "Rotate Windows")
    "b R" '(toggle-window-split         :which-key "Toggle hor/ver")
    "b i" '(semacs/org-indirect-buffer  :which-key "Indirect Buffer o. window")
    "b I" '(make-indirect-buffer        :which-key "Indirect Buffer"))


;;; Code
(my-leader-def
 :states 'normal
  "c" '(:ignore t :which-key "CODE")
  "c e" #'eval-last-sexp
  "c o" #'org-ctrl-c-ctrl-c
  "c l" #'ielm
  "c ö" #'elpy-shell-switch-to-shell
  "c m" #'emmet-expand-line
  "c n" '(elfeed                    :which-key "Elfeed")
  "c h" '(httpd-start               :which-key "Web-Server")
  "c i" '(impatient-mode            :which-key "Impatient-Mode")
  "c t" #'toggle-truncate-lines
  "c p" '(:ignore t :which-key "Python")
  "c p b" '(python-shell-send-buffer :which-key "Send buffer to shell")
  "c p s" '(python-shell-switch-to-shell :which-key "Open Python shell")
  "c p v" '(pyenv-activate-current-project :which-key "Activate Pyenv project")
  "c j" '(:ignore t :which-key "JS")
  "c j b" '(js-comint-send-buffer :which-key "Execute Buffer")
  "c j r" '(js-comint-send-region :which-key "Execute Region")
  "c j s" '(js-comint-send-last-sexp :which-key "Execute last Sexp")
  "c j n" '(js-comint-reset-repl :which-key "Reset Repl")
  "c j c" '(js-comint-clear :which-key "Clear Repl")
  "c j f" '(js-comint-load-file :which-key "Load Files")
  ;"c j b" '(js-comint :which-key "Execute Buffer")
)

;;; EMMS
(my-leader-def
  :states 'normal
  "e" '(:ignore t :which-key "Emms")
  "e e" '(emms-play-m3u-playlist :which-key "Play Playlist")
  "e s" '(emms-show :which-key "Show Playlist")
  "e g" '(emms-playlist-mode-go :which-key "Playlist")
  "e p" '(emms-pause :which-key "Pause")
  "e s" '(emms-stop :which-key "Stop")
  "e n" '(emms-next :which-key "Next Track"))

;;; File
(my-leader-def
 :states 'normal
  "f" '(:ignore t                           :which-key "FILE")
  "f c" '(connect-remote                    :which-key "Connect to Raspbery")
  "f C" '(copy-file                         :which-key "Copy file")
  "f d" '(delete-file                       :which-key "Delete file")
  "f D" '(crux-delete-buffer-and-file       :which-key "Delete buffer and file")
  "f f" '(find-file                         :which-key "find-file")
  "f F" '(dired-other-window                :which-key "Dired other window")
  "f j" '(dired-jump                        :which-key  "Jump to file in Dired")
  "f o" '(consult-org-heading               :which-key "Consult Org Headings")
  "f P" '(semacs/visit-init.el              :which-key "Open Init.el")
  "f p" '(semacs/visit-config.org           :which-key "Open Init.org")
  "f a" '(semacs/visit-abbrev.org           :which-key "Open Abbrev.org")
  "f r" '(helm-recentf                      :which-key "Recent files")
  "f R" '(crux-rename-buffer-and-file       :which-key "Rename buffer and file")
  "f S" '(write-file                        :which-key "Save file as...")
  "f t" '(semacs/visit-templates            :which-key "Open Templates")
  "f r" '(counsel-recentf                   :which-key "Recent files")
  "f R" '(crux-rename-buffer-and-file       :which-key "Rename buffer and file")
  "f u" '(sudo-edit-find-file               :which-key "Sudo find file")
  "f U" '(sudo-edit                         :which-key "Sudo edit file")
  "f v" '(crux-recentf-find-file            :which-key "Recently visit file")
)

;;; Git
(my-leader-def
 :states 'normal
  "g" '(:ignore t :which-key "GIT")
  "g g"  'magit-status
  "g d"  'magit-diff-unstaged
  "g c"  'magit-branch-or-checkout
  "g l"   '(:ignore t :which-key "log")
  "g l c" 'magit-log-current
  "g l f" 'magit-log-buffer-file
  "g b"  'magit-branch
  "g P"  'magit-push-current
  "g p"  'magit-pull-branch
  "g f"  'magit-fetch
  "g F"  'magit-fetch-all
  "g r"  'magit-rebase
)

;;; Help
(my-leader-def
 :states 'normal
  "h" '(:ignore t :which-key "HELP")
  "h h" #'discover-my-major
  "h v" #'helpful-variable
  "h f" #'helpful-function
  "h k" #'helpful-key
  "h s" #'helpful-symbol
  "h p" #'helpful-at-point
  "h b" #'describe-bindings
  "h w" #'which-key-show-full-keymap
  "h y" '(yas-describe-tables :which-key "YaSnippet Table")
  "h c" '(list-colors-display :which-key "Emacs Colors")
  "h a" #'apropos
  "h m" #'view-echo-area-messages
  "h e" #'toggle-debug-on-error
  "h F" #'describe-face
  "h K" #'which-key-show-keymap
  "h p" #'describe-package
  "h d" '(:ignore t :which-key "define-word")
  "h d w" #'define-word
  "h d p" #'define-word-at-point
)

;;; Insert
(my-leader-def
 :states 'normal
  "i" '(:ignore t :which-key "insert")
  "i w" '(org-cliplink                       :which-key "Insert Webside with desc.")
  "i s" '(org-store-link                     :which-key "Store Link")
  "i l" '(org-insert-link                    :which-key "Insert Link")
  "i f" '(semacs/copy-file-name-to-clipboard :which-key "Copy File Name")
  "i n" '(semacs/insert-filename-as-heading  :which-key "Insert File Name")
  "i d" '(saa/today                          :which-key "Insert Date")
  "i c" '(clipmon-autoinsert-toggle          :which-key "Clipmon autoinsrt")
  "i k" '(browse-kill-ring                   :which-key "Browse Kill-Ring")
  "i r" '(org-rich-yank                      :which-key "Yank with Source Block")
)

;;; Literature
(my-leader-def
  :states 'normal
"l" '(:ignore t :which-key "literatur")
"l l" '(orb-insert-link :which-key "Orb insert Link")
"l h" '(helm-bibtex :which-key "HelmBibtex Literatur")
"l n" '(org-noter :which-key "Org Note-PDF work")
"l b" '(dmenu-switch-buffer :which-key "Find file")
"l J" '(dmenu-ag :which-key "Find file wih args")
)

;;; Macro
(my-leader-def
 :states 'normal
  "m" '(:ignore t :which-key "macro")
  "m s" '(kmacro-start-macro :which-key "Macro Start")
  "m e" '(kmacro-end-macro :which-key "Macro End")
  "m n" '(kmacro-name-last-macro :which-key "Macro Name")
  "m u" '(kmacro-end-and-call-macro :which-key "Call Last Macro")
)

;;; Notes
(my-leader-def
 :states 'normal
  "n" '(:ignore t :which-key "notes")
  "n e" '(elfeed :which-key "Elfeed")
  "n c" '(calibredb :which-key "Calibre")
  "n t" '(org-transclusion-mode :which-key "Org TC Mode")
  "n l" '(org-transclusion-make-from-link :which-key "Org TC from Link")
  "n r" '(org-transclusion-refresh :which-key "Org TC Refresh")
  "n p" '(org-transclusion-promote-subtree :which-key "Org TC promote")
  "n d" '(org-transclusion-demote-subtree :which-key "Org TC demote")
)

;;; Org
(my-leader-def
 :states 'normal
  "o" '(:ignore t :which-key "org")
  "o A" '(consult-org-agenda              :which-key "Consult Org Agenda")
  "o a" '(org-agenda-file-to-front        :which-key "Add Agenda File")
  "o c" '(org-capture                     :which-key "Org Capture")
  "o s" '(consult-org-heading             :which-key "Consult Org Headings")
  "o p" '(org-set-property                :which-key "Set property")
  "o I" '(org-insert-structure-template   :which-key "STRU TEMPLATES")
  "o t" '(org-set-tags-command            :which-key "Set tags")
  "o d" '(org-treeusage-mode              :which-key "Treeusage")
  "o 4" #'unpackaged/org-mark-read-only   :which-key "Schreibschutz erstellen"
  "o 7" #'unpackaged/org-remove-read-only :which-key "Schreibschutz löschen"
  "o r" '(org-remove-file :which-key "Remove Agenda File")
  "o R" '(org-reload :which-key "Remove Agenda File")
  "o o" #'org-agenda
  "o s" #'org-schedule
  "o n" #'org-add-note
  "o S" '(org-sort                                 :which-key "Org-Sort")
  "o i" #'org-toggle-inline-images
  "o T" #'org-table-toggle-coordinate-overlays
  "o +" '(org-narrow-to-subtree :which-key "Show-Subtree")
  "o -" '(widen :which-key "Widen-Subtree")
  "o v" #'org-tags-view)

;;; Project
(my-leader-def
 :states 'normal
  "p" '(:ignore t :which-key "project")
  "p p" #'buffer-flip-forward
  "p p" #'projectile-command-map
  "p f" '(projectile-find-file :which-key "Projectile find file")
)

;;; Roam
(my-leader-def
 :states 'normal
  "r" '(:ignore t :which-key "roam")
  "r a" '(org-roam-alias-add :which-key "Add Alias")
  "r A" '(org-roam-alias-remove :which-key "Remove Alias")
  "r b" '(org-roam-buffer-toggle :which-key "Roam Buffer")
  "r d" '(org-id-get-create :which-key "Create ID")
  "r D" #'org-roam-dailies-capture-today
  "r f" '(org-roam-node-find :which-key "Find Note")
  "r F" #'consult-org-roam-file-find
  "r g" #'org-roam-graph
  ;;"r i" #'org-roam-node-insert
  "r I" #'org-roam-node-insert-immediate
  "r i" #'vulpea-insert
  "r l" #'org-insert-link
  "r m" #'org-roam-capture
  "r p" '(semacs/visit-roam-zettelkasten :which-key "Roam Index")
  "r s" '(semacs/org-occur-tag-search :which-key "Org Tag Search")
  "r U" '(org-roam-update-org-id-locations         :which-key "Update ID location")
  "r t" #'org-roam-tag-add
  "r T" #'org-roam-tag-remove
  "r t" '(org-roam-buffer-toggle-display :which-key "Maximze Buffer")
  "r u" '(org-roam-unlinked-references :which-key "Unlineked Refernces")
)

;;; Search
(my-leader-def
 :states 'normal
  "s" '(:ignore t :which-key "search")
  "s s" '(swiper :which-key "Swiper - Search in file")
  "s d" '(deadgrep :which-key "Deadgrep - Search in file")
  "s r" '(consult-ripgrep :which-key "Consult-Ripgrep")
  "s f" '(consult-find :which-key "Consult Find")
  "s h" '(helm-find :which-key "Helm find")
  "s o" '(consult-org-heading               :which-key "Consult Org Headings")
  "s b" '(helm-bookmarks :which-key "Helm Bookmark")
  "s g" '(semacs/org-roam-rg-search :which-key "Roam RG Search")
  "s w" '(webjump :which-key "Visit Webside")
  "s t" '(google-translate-query-translate  :which-key "Translate")
  "s a" '(affe-grep :which-key "Affe")
  "s p" '(ripgrep-regexp :which-key "Search all files with ripgrep")
)

;;; Terminal
(my-leader-def
 :states 'normal
  "t" '(:ignore t :which-key "terminal")
  "t b" '(run-bash :which-key "Bash")
  "t w" '(run-cmdexe :which-key "Cmd")
  "t c" '(shell-command :which-key "Execute shell command")
  "t p" '(run-powershell :which-key "Powershell")
  "t e" '(eshell :which-key "Eshell")
  "t t" '(term :which-key "Term")
  "t s" '(shell :which-key "Shell")
)

;;; View
(my-leader-def
 :states 'normal
  "v" '(:ignore t :which-key "view")
  "v t" '(counsel-load-theme :which-key "Change Theme")
  "v p" '(variable-pitch-mode :which-key "Pitch Mode")
)

;;; Writing
(my-leader-def
 :states 'normal
  "w" '(:ignore t                           :which-key "writing")
  "w w" #'semacs/flyspell-and-whitespace-mode
  "w c" #'flyspell-correct-wrapper
)

;;; X-Spotify
(my-leader-def
 :states 'normal
  "x" '(:ignore t                           :which-key "Music")
  "x s" '(counsel-spotify-search-track      :which-key "Search Tracks")
  "x n" '(counsel-spotify-next              :which-key "Next Track")
  "x p" '(counsel-spotify-previous          :which-key "Previous Track")
  "x t" '(counsel-spotify-toggle-play-pause :which-key "Toggle Play/Pause")
  "x t" '(counsel-spotify-play              :which-key "Play Tracks"))


;;; Quit
(my-leader-def
 :states 'normal
  "q" '(:ignore t                           :which-key "quit")
  "q r" '(restart-emacs                     :which-key "Restart Emacs")
  "q n" '(restart-emacs-start-new-emacs     :which-key "New Instant")
  "q q" '(evil-quit                         :which-key "Quit Emacs"))


;;; Which-Key
;;; [[https://github.com/justbur/emacs-which-key][Which-Key]] is a minor mode for Emacs that displays the key bindings following your currently entered incomplete command (a prefix) in a popup.

(use-package which-key
  :config
  (which-key-mode)
  ;(which-key-setup-minibuffer)

  :custom
  (which-key-max-display-columns 14)
  (which-key-min-display-lines 4)
  (which-key-sort-order 'which-key-key-order-alpha)
  ;; Allow C-h to trigger which-key before it is done automatically
  (which-key-show-early-on-C-h t)
  ;; make sure which-key doesn't show normally but refreshes quickly after it is
  ;; triggered.
  (which-key-idle-delay 1.2)
  (which-key-idle-secondary-delay 0.05)
  (which-key-paging-prefixes '("C-x"))
  (which-key-allow-evil-operators t)
  (which-key-max-description-length 27)
  (which-key-show-operator-state-maps t))


;;; Don't arrow
(use-package emacs
  :disabled
  :preface
  (defun rune/dont-arrow-me-bro ()
    (interactive)
    (message "Arrow keys are bad, you know?"))

    ;; Disable arrow keys in normal and visual modes
    (define-key evil-normal-state-map (kbd "<left>") 'rune/dont-arrow-me-bro)
    (define-key evil-normal-state-map (kbd "<right>") 'rune/dont-arrow-me-bro)
    (define-key evil-normal-state-map (kbd "<down>") 'rune/dont-arrow-me-bro)
    (define-key evil-normal-state-map (kbd "<up>") 'rune/dont-arrow-me-bro)
    (evil-global-set-key 'motion (kbd "<left>") 'rune/dont-arrow-me-bro)
    (evil-global-set-key 'motion (kbd "<right>") 'rune/dont-arrow-me-bro)
    (evil-global-set-key 'motion (kbd "<down>") 'rune/dont-arrow-me-bro)
    (evil-global-set-key 'motion (kbd "<up>") 'rune/dont-arrow-me-bro))

;;; core-keybinds.el ends here
