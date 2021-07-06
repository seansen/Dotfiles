;;; core-keybinds.el --- Loads afer the early-init file -*- lexical-binding: t; -*-

;;; core.el --- Loads afer the init file -*- lexical-binding: t; -*-

;;; Commentary:
;;; - Which-key
;;; - Evil
;;; - General

;;; Code

;;; Which-Key
(use-package which-key
  :init
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-max-display-columns 6
        which-key-min-display-lines 4
        which-key-side-window-slot -10)
(setq which-key-max-description-length 27)
  :custom
  (which-key-setup-minibuffer)
  (which-key-setup-side-window-bottom)
  (which-key-enable-extended-define-key t)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-side-window-max-width 0.53)
  (which-key-idle-delay 0.05)
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))

;;; Evil-Keybindings
(use-package evil
  :init
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    (setq evil-want-C-i-jump nil)
    (setq evil-move-cursor-back nil)
  :config
    (evil-mode 1)
    (setq evil-normal-state-cursor '("#c4b5f0" box))
    (setq evil-emacs-state-cursor '("orange" box))
    (setq evil-visual-state-cursor '("green" box))
    (setq evil-insert-state-cursor '("red" bar))
    (setq evil-replace-state-cursor '("yellow" bar))
    (setq evil-operator-state-cursor '("blue" hollow))
)

;;; Evil-Surround
;; It's easiest to explain with examples. Press =cs"'= inside
(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode))

;;; Evil-Escape
;; Customizable key sequence to escape from insert state and everything else in Emacs.
(use-package evil-escape
  :after evil
  :config
    (evil-escape-mode t)
    (setq evil-move-cursor-back nil
          evil-escape-key-sequence "jk"
          evil-escape-delay 0.63
          evil-escape-unordered-key-sequence t))

;;; Evil-Easymotion
;; With evil-easymotion you can invoke SPCj, and this plugin will put a target character on every possible position.
(use-package evil-easymotion
  :after evil
  :config
    (evilem-default-keybindings "ö"))

;;; Evil-Commentary
;; Intends to make it easy to comment out (lines of) code.
(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode))

;;; Evil-Goggles
;; Evil-goggles-mode displays a visual hint when editing with evil.
(use-package evil-goggles
  :after evil
  :config
     (evil-goggles-mode)
     (evil-goggles-use-diff-faces))

;;; Evil-Org
;; Provides integration between Evil and Org through various means such as contextual keybindings.
(use-package org-evil
   :after evil)

;;; Evil-Collection
;;  A collection???
(use-package evil-collection
   :after evil
   :custom
     (evil-collection-setup-minibuffer t)
     (evil-want-keybinding nil)
   :init
   (evil-collection-init))
;;; Evil-Exchange
;;

(use-package evil-exchange
  :after evil
  :custom
  setq evil-exchange-key (kbd "zx")
  (evil-exchange-install))

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

;;; Not evil modes!
;; There are many modes that are not centered about text-manipulation.

(mapc (lambda (mode)
  (evil-set-initial-state mode 'emacs))
    '(;;elfeed-show-mode
      ;;elfeed-search-mode
      calibredb-search-mode
      calibredb-show-mode
      eshell-mode
      forge-pullreq-list-mode
      forge-topic-list-mode
      dired-mode
      tide-references-mode
      image-dired-mode
      bufler-mode
      image-dired-thumbnail-mode
      eww-mode))

;;
;;; Evil bindings

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
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)

;; Quit keyboard
(global-set-key (kbd "<escape>")
                 'keyboard-escape-quit)                    ;Make ESC quit prompts

;;
;;; General bindings
(use-package general
  :config
    (general-define-key
      "M-x" 'counsel-M-x                               ; replace default M-x with ivy backend
      "C-s" 'swiper                                    ; search for string in current buffer
      "C-;" 'flyspell-correct-wrapper                  ;
      "C-M-<return>" 'org-open-at-point                ; open link in org-mode
      "M-o" 'org-open-at-point                         ; open link in org-mode
      "M-s-<right>" #'buffer-flip-backward             ;
      "M-s-<left>" #'buffer-flip-forward               ;
      "M-s-<up>" #'semacs/move-line-up                 ;
      "M-s-<down>" #'semacs/move-line-down             ;
      "<f5>" 'counsel-ibuffer                          ;
      "<f6>" 'clipmon-autoinsert-toggle                ;
      "<f9>" 'saa/flyspell-and-whitespace-mode
      "<f10>" 'other-window                            ;
      "M-p" #'other-window                             ;
      "M-s-<tab>" 'other-window                        ; open link in org-mode
      "C-c _" 'undo-tree-visualize                     ;
      "C-c P" 'saa/copy-file-name-to-clipboard)        ;

    (general-create-definer my-leader-def
      :keymaps 'override
      :prefix "SPC"
      :global-prefix "C-SPC"
      :prefix "SPC"))

;;
;;; Leader-Maps
;;  - Buffer
;;  - Code
;;  - Export
;;  - Git
;;  - Help
;;  - Inser
;;  - Macro
;;  - Note
;;  - Org
;;  - Projekt
;;  - Roam
;;  - Search
;;  - Terminal
;;  - View
;;  - Writing
;;  - Quit

;; Buffer
(my-leader-def
  :states 'normal
"b" '(:ignore t :which-key "buffer")
"b b" '(bufler :which-key "List all Buffers")
"b k" '(kill-current-buffer :which-key "Kill Buffer")
"b K" '(kill-buffer :which-key "Kill Buffer from List")
"b v" '(evil-window-vsplit :which-key "Split Window vertical")
"b h" '(evil-window-split :which-key "Split Window horizontaly")
"b r" '(ssa/rotate-windows :which-key "Rotate Windows")
"b t" '(org-roam-buffer-toggle-display :which-key "Maximze Buffer")
"b s" '(evil-save :which-key "Save Buffer")
)

;; Code
(my-leader-def
 :states 'normal
  "c" '(:ignore t :which-key "code")
  "c e" #'eval-last-sexp
  "c o" #'org-ctrl-c-ctrl-c
  "c l" #'ielm
  "c ö" #'elpy-shell-switch-to-shell
  "c m" #'emmet-expand-line
  "c n" '(elfeed :which-key "Elfeed")
  "c h" '(httpd-start :which-key "Web-Server")
  "c i" '(impatient-mode :which-key "Impatient-Mode")
  "c t" #'toggle-truncate-lines
  "c j" '(:ignore t :which-key "JS")
  "c j b" '(js-comint-send-buffer :which-key "Execute Buffer")
  "c j r" '(js-comint-send-region :which-key "Execute Region")
  "c j s" '(js-comint-send-last-sexp :which-key "Execute last Sexp")
  "c j n" '(js-comint-reset-repl :which-key "Reset Repl")
  "c j c" '(js-comint-clear :which-key "Clear Repl")
  "c j f" '(js-comint-load-file :which-key "Load Files")
  ;"c j b" '(js-comint :which-key "Execute Buffer")
)

;; Delete
(my-leader-def
 :states 'normal
  "d" '(:ignore t :which-key "delete")
  "d l" '(saa/delete-line :which-key "Delete line")
  "d d" '(saa/delete-whole-line :which-key "Delete line")
  "d s" '(saa/smart-delete-line :which-key "Delete line")
)

;; Export
(my-leader-def
 :states 'normal
  "e" '(:ignore t :which-key "export")
  "e e" #'org-html-export-to-html
  "e m" #'org-pandoc-export-to-gfm-and-open
  "e o" #'saa/markdown-convert-buffer-to-org
)

;; File
(my-leader-def
 :states 'normal
  "f" '(:ignore t :which-key "file")
  "f f" '(dired :which-key "Dired")
  "f F" '(dired-other-window :which-key "Dired other window")
  "f o" '(crux-open-with :which-key "Open extern")
  "f r" '(helm-recentf :which-key "Recent files")
  "f R" #'(crux-rename-buffer-and-file :which-key "Rename buffer and file")
  "f p" #'semacs/visit-core-settings
  "f P" #'semacs/visit-core-config
  "f v" '(crux-recentf-find-file :which-key "Recently visit file")
  "f s" '(evil-save :which-key "Save File")
)

;; Git
(my-leader-def
 :states 'normal
  "g" '(:ignore t :which-key "git")
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


;; Help
(my-leader-def
 :states 'normal
  "h" '(:ignore t :which-key "help")
  "h v" #'helpful-variable
  "h f" #'helpful-function
  "h k" #'helpful-key
  "h s" #'helpful-symbol
  "h p" #'helpful-at-point
  "h b" #'describe-bindings
  "h w" #'which-key-show-full-keymap
  "h y" '(yas-describe-tables :which-key "YaSnippet Table")
  "h a" #'apropos
  "h e" #'view-echo-area-messages
  "h F" #'describe-face
  "h K" #'describe-keymap
  "h p" #'describe-package
  "h d" '(:ignore t :which-key "define-word")
  "h d w" #'define-word
  "h d p" #'define-word-at-point
)

;; Insert
(my-leader-def
 :states 'normal
  "i" '(:ignore t :which-key "insert")
  "i y" #'counsel-yank-pop
  "i l" #'org-cliplink
  "i n" #'saa/insert-filename-as-heading
  "i f" #'saa/copy-file-name-to-clipboard
  "i d" '(saa/today :which-key "Insert Date")
  "i c" '(clipmon-autoinsert-toggle :which-key "Clipmon autoinsrt")
  "i p" '(popup-kill-ring :which-key "Popup Kill-Ring")
  "i k" '(browse-kill-ring :which-key "Browse Kill-Ring")
  "i r" '(org-rich-yank :which-key "Yank with Source Block")
  "i t" '(clipmon-autoinsert-toggle :which-key "Clipmon Toggle")
)

;; Macro
(my-leader-def
 :states 'normal
  "m" '(:ignore t :which-key "macro")
  "m s" '(kmacro-start-macro :which-key "Macro Start")
  "m e" '(kmacro-end-macro :which-key "Macro End")
  "m n" '(kmacro-name-last-macro :which-key "Macro Name")
  "m u" '(kmacro-end-and-call-macro :which-key "Call Last Macro")
)

;; Notes
(my-leader-def
 :states 'normal
  "n" '(:ignore t :which-key "notes")
  "n e" '(elfeed :which-key "Elfeed")
  "n c" '(calibredb :which-key "Calibre")
)

;; Org
(my-leader-def
 :states 'normal
  "o" '(:ignore t :which-key "org")
  "o a" #'org-agenda
  "o s" #'org-schedule
  "o n" #'org-add-note
  "o c" #'org-table-toggle-coordinate-overlays
  "o n" #'org-add-note
  "o i" #'org-toggle-inline-images
  "o t" #'org-insert-structure-template
)

;; Project
(my-leader-def
 :states 'normal
  "p" '(:ignore t :which-key "project")
  "p p" #'buffer-flip-forward
  "p p" #'projectile-command-map
  "p f" '(projectile-find-file :which-key "Projectile find file")
)

;; Roam
(my-leader-def
 :states 'normal
  "r" '(:ignore t :which-key "roam")
  "r r" #'org-roam
  "r f" #'org-roam-find-file
  "r g" #'org-roam-graph
  "r i" #'org-roam-insert
  "r m" #'org-roam-insert-immediate
  "r l" #'org-insert-link
  "r c" #'org-cliplink
  "r n" #'saa/rename-current-buffer-file
  "r u" '(org-roam-unlinked-references :which-key "Unlineked Refernces")
  "r p" '(saa/visit-roam-zettelkasten :which-key "Roam Index")
  "r b" '(org-roam-switch-to-buffer :which-key "Roam Buffer")
  "r t" '(org-roam-buffer-toggle-display :which-key "Maximze Buffer")
)

;; Search
(my-leader-def
 :states 'normal
  "s" '(:ignore t :which-key "search")
  "s s" '(swiper :which-key "Search in file")
  "s r" '(helm-org-rifle :which-key "?")
  "s h" '(helm-find :which-key "?")
  "s b" '(helm-bookmarks :which-key "Bookmark")
  "s w" '(webjump :which-key "Visit Webside")
  "s d" '(deft :which-key "Search Org-Files")
  "s a" '(evil-avy-goto-char :which-key "Avy go to Word")
)

;; Terminal
(my-leader-def
 :states 'normal
  "t" '(:ignore t :which-key "terminal")
  "t b" '(run-bash :which-key "Bash")
  "t c" '(run-cmdexe :which-key "Cmd")
  "t p" '(run-powershell :which-key "Powershell")
  "t e" '(eshell :which-key "Eshell")
  "t t" '(term :which-key "Term")
  "t s" '(shell :which-key "Shell")
)
;; View
(my-leader-def
 :states 'normal
  "v" '(:ignore t :which-key "view")
  "v t" '(counsel-load-theme :which-key "Change Theme")
  "v p" '(variable-pitch-mode :which-key "Pitch Mode")
)

;; Writing
(my-leader-def
 :states 'normal
  "w" '(:ignore t :which-key "quit")
  "w w" #'saa/flyspell-and-whitespace-mode
  "w c" #'flyspell-correct-wrapper
)

;; Quit
(my-leader-def
 :states 'normal
  "q r" '(restart-emacs :which-key "Restart Emacs")
  "q n" '(restart-emacs-start-new-emacs :which-key "New Instant")
  "q q" '(evil-quit :which-key "Quit Emacs")
	)

;;; core-keybinds.el ends here
