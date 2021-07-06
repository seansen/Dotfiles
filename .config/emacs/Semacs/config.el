(defconst semacs-version "1.0.3"
  "Current version of Semacs.")

(defun setup-load-path ()
  (defconst semacs-emacs-dir user-emacs-directory
    "The path to the currently loaded .emacs.d directory. Must end with a slash.")

  (defconst semacs-core-dir (concat semacs-emacs-dir "core/")
    "The root directory of Semacs's core files. Must end with a slash.")

  (defconst semacs-modules-dir (concat semacs-emacs-dir "modules/")
    "The root directory for Semacs's modules. Must end with a slash.")

  (defconst semacs-private-dir (concat semacs-emacs-dir "config/")
    "Where your private configuration is placed.")

  (defconst semacs-local-dir (concat semacs-emacs-dir ".local/")
	  "Root directory for local storage.")

  (defconst semacs-cache-dir (concat semacs-emacs-dir ".local/cache/")
    "Directory for volatile local storage.
     Use this for files that change often, like cache files. Must end with a slash.")

  (defconst semacs-etc-dir (concat semacs-emacs-dir ".local/etc/")
	 "Directory for non-volatile local storage.
    Use this for files that don't change much, like server binaries, external
    dependencies or long-term shared data. Must end with a slash.")

  (defconst semacs-docs-dir (concat semacs-emacs-dir "docs/")
    "Where Semacs's documentation files are stored. Must end with a slash.")

  ;; Add folders to load path
  (add-to-list 'load-path semacs-core-dir)
  (add-to-list 'load-path semacs-modules-dir)
  (add-to-list 'load-path semacs-cache-dir)
  (add-to-list 'load-path semacs-local-dir)
  (add-to-list 'load-path semacs-etc-dir)
  (add-to-list 'load-path semacs-private-dir)
  (add-to-list 'load-path semacs-docs-dir)
)

(setup-load-path)

(defconst EMACS27+   (> emacs-major-version 26))
(defconst EMACS28+   (> emacs-major-version 27))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))

(when (and IS-WINDOWS (null (getenv-internal "HOME")))
(setenv "HOME" (getenv "USERPROFILE"))
(setq abbreviated-home-dir nil))

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

(setq locale-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)

(setq ad-redefinition-action 'accept)

(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(setq gc-cons-threshold 64000000)

(add-hook 'after-init-hook #'(lambda ()
  (setq gc-cons-threshold 1200000)))

(add-hook 'dashboard-after-initialize-hook
;(add-hook 'emacs-startup-hook
  (lambda ()
    (message "*** Semacs loaded in %s with %d garbage collections."
      (format "%.2f seconds"
        (float-time
        (time-subtract after-init-time before-init-time)))
      gcs-done)))

(setq auto-mode-case-fold nil)

(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

(setq bidi-inhibit-bpa t)  ; Emacs 27 only

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)

(setq ffap-machine-p-known 'reject)

(setq frame-inhibit-implied-resize t)

(setq idle-update-delay 1.0)

(setq inhibit-compacting-font-caches t)

(setq redisplay-skip-fontification-on-input t)

(when IS-WINDOWS
  (setq w32-get-true-file-attributes nil   ; decrease file IO workload
        w32-pipe-read-delay 0              ; faster ipc
        w32-pipe-buffer-size (* 64 1024))) ; read more at a time (was 4K)

(unless IS-MAC   (setq command-line-ns-option-alist nil))
(unless IS-LINUX (setq command-line-x-option-alist nil))

(setq package-enable-at-startup nil)
;(setq package-user-dir (concat semacs-local-dir "elpa/"))
(setq package-user-dir "~/Coding/elisp/Semacs/.local/elpa/")

(require 'package)

(setq my-package-archives '(("melpa-stable" . "http://stable.melpa.org/packages/")
                            ("org" . "http://orgmode.org/elpa/")
                            ("melpa" . "https://melpa.org/packages/")))

(setq package-archives (append package-archives
                               my-package-archives))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;(if (not server-mode)
;    (server-start nil t))
(load "server")
(unless (server-running-p) (server-start))
(if (daemonp)
    (message "Loading in the daemon!")
    (message "Loading in regular Emacs!"))

(setq user-full-name "Sean Avery")
(setq user-mail-address "seanavery@gmx.net")
(setq calendar-latitude 52.5)
(setq calendar-longitude 13.4)
(setq calendar-location-name "Berlin, DE")

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
    (setq evil-test-state-cursor '("black" bar))
    (setq evil-operator-state-cursor '("blue" hollow))
)

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode))

(use-package evil-escape
  :after evil
  :config
    (evil-escape-mode t)
    (setq evil-move-cursor-back nil
          evil-escape-key-sequence "jk"
          evil-escape-delay 0.63
          evil-escape-unordered-key-sequence t))

(use-package evil-easymotion
  :after evil
  :config
    (evilem-default-keybindings "ö"))

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode))

(use-package evil-goggles
  :after evil
  :config
     (evil-goggles-mode)
     (evil-goggles-use-diff-faces))

(use-package org-evil
   :after evil)

(use-package evil-collection
   :after evil
   :custom
     (evil-collection-setup-minibuffer t)
     (evil-want-keybinding nil)
   :init
   (evil-collection-init))

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

(use-package evil-nerd-commenter
  :bind ("s-/" . evilnc-comment-or-uncomment-lines))

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

(use-package general
  :config
    (general-define-key
      "C-<shift>" 'set-mark-command ;;;; DDDDDDDDDDDDDDDDDD
      "M-x" 'counsel-M-x                               ; replace default M-x with ivy backend
      "C-s" 'swiper                                    ; search for string in current buffer
      "C-;" 'flyspell-correct-wrapper                  ;
      "C-K" 'kill-dired-buffers                        ;
      "C-M-<return>" 'org-open-at-point                ; open link in org-mode
      "M-o" 'org-open-at-point                         ; open link in org-mode
      "M-s-<right>" 'buffer-flip-backward             ;
      "M-s-h" 'buffer-flip-backward             ;
      "M-s-<left>" 'buffer-flip-forward               ;
      "M-s-l" 'buffer-flip-forward               ;
      "M-s-<up>" 'semacs/move-line-up                 ;
      "M-s-k" 'semacs/move-line-up                 ;
      "M-s-<down>" 'semacs/move-line-down             ;
      "M-s-j" 'semacs/move-line-down             ;
      "<f5>" 'bufler                          ;
      "<f6>" 'clipmon-autoinsert-toggle                ;
      "<f9>" 'saa/flyspell-and-whitespace-mode
      "<f10>" 'other-window                            ;
      "M-p" 'other-window                             ;
      "M-s-<tab>" 'other-window                        ; open link in org-mode
      "C-c _" 'undo-tree-visualize                     ;
      "C-c P" 'saa/copy-file-name-to-clipboard)        ;

    (general-create-definer my-leader-def
      :keymaps 'override
      :prefix "SPC"
      :global-prefix "C-SPC"
      :prefix "SPC"))

(global-set-key (kbd "<escape>")
                 'keyboard-escape-quit)                    ;Make ESC quit prompts

(my-leader-def
  :states 'normal
"b" '(:ignore t :which-key "buffer")
"b b" '(counsel-ibuffer :which-key "List all Buffers")
"b k" '(kill-current-buffer :which-key "Kill Buffer")
"b K" '(nuke-all-buffers :which-key "Kill all Buffers")
"b v" '(evil-window-vsplit :which-key "Split Window vertical")
"b V" '(dired-other-window :which-key "Dired other window")
"b h" '(evil-window-split :which-key "Split Window horizontaly")
"b r" '(semacs/rotate-windows :which-key "Rotate Windows")
"b t" '(org-roam-buffer-toggle-display :which-key "Maximze Buffer")
"b s" '(evil-save :which-key "Save Buffer")
)

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

(my-leader-def
 :states 'normal
  "d" '(:ignore t :which-key "delete")
  "d l" '(saa/delete-line :which-key "Delete line")
  "d d" '(saa/delete-whole-line :which-key "Delete line")
  "d s" '(saa/smart-delete-line :which-key "Delete line")
)

(my-leader-def
 :states 'normal
  "e" '(:ignore t :which-key "export")
  "e e" #'org-html-export-to-html
  "e m" #'org-pandoc-export-to-gfm-and-open
  "e o" #'saa/markdown-convert-buffer-to-org
)

(my-leader-def
 :states 'normal
  "f" '(:ignore t :which-key "file")
  "f c" '(connect-remote :which-key "Connect to Raspbery")
  "f C" '(copy-file :which-key "Copy file")
  "f D" '(delete-file :which-key "Delete file")
  "f f" '(dired :which-key "Dired")
  "f F" '(dired-other-window :which-key "Dired other window")
  "f o" '(crux-open-with :which-key "Open extern")
  "f P" '(semacs/visit-init.el :which-key "Open Init.el")
  "f p" '(semacs/visit-config.org :which-key "Open Init.org")
  "f r" '(helm-recentf :which-key "Recent files")
  "f R" '(crux-rename-buffer-and-file :which-key "Rename buffer and file")
  "f s" '(evil-save :which-key "Save File")
  "f s" '(save-buffer :which-key "Save file")
  "f S" '(write-file :which-key "Save file as...")
  "f r" '(counsel-recentf :which-key "Recent files")
  "f R" '(rename-file :which-key "Rename file")
  "f u" '(sudo-edit-find-file :which-key "Sudo find file")
  "f U" '(sudo-edit :which-key "Sudo edit file")
  "f v" '(crux-recentf-find-file :which-key "Recently visit file")

)

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

(my-leader-def
  :states 'normal
"j" '(:ignore t :which-key "dmenu")
"j j" '(dmenu-find-file :which-key "Find file")
"j f" '(dmenu-find-file :which-key "Find file")
"j k" '(dmenu-ag-in-file :which-key "Find file")
"j b" '(dmenu-switch-buffer :which-key "Find file")
"j J" '(dmenu-ag :which-key "Find file wih args")
)

(my-leader-def
 :states 'normal
  "m" '(:ignore t :which-key "macro")
  "m s" '(kmacro-start-macro :which-key "Macro Start")
  "m e" '(kmacro-end-macro :which-key "Macro End")
  "m n" '(kmacro-name-last-macro :which-key "Macro Name")
  "m u" '(kmacro-end-and-call-macro :which-key "Call Last Macro")
)

(my-leader-def
 :states 'normal
  "n" '(:ignore t :which-key "notes")
  "n e" '(elfeed :which-key "Elfeed")
  "n c" '(calibredb :which-key "Calibre")
)

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

(my-leader-def
 :states 'normal
  "p" '(:ignore t :which-key "project")
  "p p" #'buffer-flip-forward
  "p p" #'projectile-command-map
  "p f" '(projectile-find-file :which-key "Projectile find file")
)

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
  "s p" '(ripgrep-reg-exp :which-key "Search all files with ripgrep")
)

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

(my-leader-def
 :states 'normal
  "v" '(:ignore t :which-key "view")
  "v t" '(counsel-load-theme :which-key "Change Theme")
  "v p" '(variable-pitch-mode :which-key "Pitch Mode")
)

(my-leader-def
 :states 'normal
  "w" '(:ignore t :which-key "writing")
  "w w" #'saa/flyspell-and-whitespace-mode
  "w c" #'flyspell-correct-wrapper
)

(my-leader-def
 :states 'normal
  "x" '(:ignore t :which-key "Music")
  "x s" '(counsel-spotify-search-track :which-key "Search Tracks")
  "x n" '(counsel-spotify-next :which-key "Next Track")
  "x p" '(counsel-spotify-previous :which-key "Previous Track")
  "x t" '(counsel-spotify-toggle-play-pause :which-key "Toggle Play/Pause")
  "x t" '(counsel-spotify-play :which-key "Play Tracks")
	)

(my-leader-def
 :states 'normal
  "q" '(:ignore t :which-key "quit")
  "q r" '(restart-emacs :which-key "Restart Emacs")
  "q n" '(restart-emacs-start-new-emacs :which-key "New Instant")
  "q q" '(evil-quit :which-key "Quit Emacs")
	)

(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))    ;Load the custom theme folder.
(add-to-list 'load-path (concat user-emacs-directory "themes/"))           ;Some themes have several variations and share code between elisp modules these modules need to be on the load path so that these themes work
(setq custom-safe-themes t)                                                ;Load themes withot asking

(use-package doom-themes
  :defer
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package circadian
  :config
  (setq circadian-themes '((:sunrise . doom-flatwhite)
                           (:sunset  . doom-dracula)))
  (circadian-setup))

(setq frame-title-format '("%b – Semacs V3"))                              ;A simple frame title
(setq icon-title-format frame-title-format)                                ;
(setq frame-resize-pixelwise t)                                            ;Don't resize the frames in steps.
(setq window-resize-pixelwise nil)                                         ;But do not resize windows pixelwise, this can cause crashes in some cases where we resize windows too quickly.
(push '(menu-bar-lines . 0)   default-frame-alist)                         ;Disable tool
(push '(tool-bar-lines . 0)   default-frame-alist)                         ;Disable menu
(push '(vertical-scroll-bars) default-frame-alist)                         ;Disable scroll-bar
(setq menu-bar-mode nil)                                                   ;Variables must be unset too, otherwise users will have to cycle them twice to re-enable them.
(setq tool-bar-mode nil)                                                  ;Variables must be unset too, otherwise users will have to cycle them twice to re-enable them.
(setq scroll-bar-mode nil)                                                 ;Variables must be unset too, otherwise users will have to cycle them twice to re-enable them.
(setq use-dialog-box nil)                                                  ;Always avoid GUI
(setq split-width-threshold 160)                                           ;Favor vertical splits over horizontal ones. Screens are usually wide.
(setq split-height-threshold nil)                                          ;Favor vertical splits over horizontal ones. Screens are usually wide.
(setq ring-bell-function #'ignore)                                           ;Ring and visibel Bell
(setq visible-bell nil)                                                    ;;
(set-frame-parameter (selected-frame) 'alpha '(100 100))                   ;Change the frame transperency.
(add-to-list 'default-frame-alist '(alpha 100 100))                        ;Change the frame transperency.
(setq window-divider-default-places t)                                     ;The native border "consumes" a pixel of the fringe on righter-most splits, window-divider" does not.
(setq window-divider-default-bottom-width 1)
(setq window-divider-default-right-width 1)
(setq mouse-yank-at-point t)                                               ;Middle-click paste at point, not at click
(add-hook 'doom-init-ui-hook #'window-divider-mode)
(when (bound-and-true-p tooltip-mode)                                      ;Don't display floating tooltips; display their contents in the echo-area, because native tooltips are ugly.
  (tooltip-mode -1))
(when IS-LINUX
  (setq x-gtk-use-system-tooltips nil))
;(setq initial-frame-alist '((left . .0)                                   ;Change the inital frame size.
;                            (width . 105)
;                            (fullscreen . fullheight)))

;;; Frame-Marginr
(setq-default left-margin-width 1 right-margin-width 1)
(set-window-buffer nil (current-buffer))

(use-package all-the-icons
  :commands (all-the-icons-octicon
             all-the-icons-faicon
             all-the-icons-fileicon
             all-the-icons-wicon
             all-the-icons-material
             all-the-icons-alltheicon)
  :preface
  (setq doom-unicode-extra-fonts
        (list "Weather Icons"
              "github-octicons"
              "FontAwesome"
              "all-the-icons"
              "file-icons"
              "Material Icons")))

(setq display-time-day-and-date t)                                                ;Display time and date
(display-time-mode 1)                                                             ;
(setq display-time-24hr-format t)                                                 ;Enable time in the mode-line with 24h format.
(display-battery-mode 1)                                                          ;Enable battery power in the mode-line.
(setq column-number-mode t)                                                       ;Display of the current line number or column number
(minibuffer-depth-indicate-mode)                                                  ;

(use-package doom-modeline
  :after eshell     ;; Make sure it gets hooked after eshell
  :custom
  (setq doom-modeline-height 15)
  (setq doom-modeline-bar-width 6)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-github nil)
  (setq doom-modeline-mu4e nil)
  (setq doom-modeline-irc nil)
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-persp-name nil))

(add-hook 'after-init-hook #'doom-modeline-mode)

; UTF-8 don’t show in modeline unless the encoding is something different
(defun doom-modeline-conditional-buffer-encoding ()
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))
(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

(use-package nyan-mode
  :config
    (setq nyan-mode t))

(straight-use-package
 '(blackout :host github :repo "raxod502/blackout"))

(setq-default window-divider-default-right-width 3)
(setq-default window-combination-resize t)                                 ;Take new window space from all other windows (not just current)
(setq-default window-divider-default-places 'right-only)

(use-package ace-window
  :config
  ;; Show the window designators in the modeline.
  (ace-window-display-mode)

   ;; Make the number indicators a little larger. I'm getting old.
  (set-face-attribute 'aw-leading-char-face nil :height 2.0 :background "black")

  (defun my-ace-window (args)
    "As ace-window, but hiding the cursor while the action is active."
    (interactive "P")
    (cl-letf
        ((cursor-type nil)
         (cursor-in-non-selected-window nil))
      (ace-window nil)))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l) "Designate windows by home row keys, not numbers.")
  (aw-background nil))

(defun semacs/rotate-windows ()
  "Rotate your windows."
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(use-package beacon
:config
(beacon-mode 1)
; this color looks good for the zenburn theme but not for the one
 (setq beacon-color "#666600")
)

(setq uniquify-buffer-name-style 'forward)
(setq confirm-nonexistent-file-or-buffer t)                                ;Confirmation for creating a new file or buffer.
(setq-default display-line-numbers-type 'relative)

;; ...but don't do any wrapping by default. It's expensive. Enable
;; `visual-line-mode' if you want soft line-wrapping. `auto-fill-mode' for hard
;; line-wrapping.
(setq-default truncate-lines t)

;; If enabled (and `truncate-lines' was disabled), soft wrapping no longer
;; occurs when that window is less than `truncate-partial-width-windows'
;; characters wide. We don't need this, and it's extra work for Emacs otherwise,
;; so off it goes.
(setq truncate-partial-width-windows nil)

(setq default-fill-column 80)

(setq line-move-visual t)

(setq-default fill-column 80)

(global-hl-line-mode t)

(show-paren-mode t)

(setq-default word-wrap t)

;(setq-default smartparense-mode t)
 (setq-default  display-line-numbers-type 'relative)

;;; Package

;;; Code:
(use-package auto-dim-other-buffers
  :config
  (require 'auto-dim-other-buffers)
  (add-hook 'after-init-hook (lambda ()
    (when (fboundp 'auto-dim-other-buffers-mode)
    (auto-dim-other-buffers-mode t)))))

(use-package rainbow-delimiters
  :defer t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package info-colors
  :defer t
  :commands (info-colors-fontify-node))
  (add-hook 'Info-selection-hook 'info-colors-fontify-node)
  (add-hook 'Info-mode-hook #'mixed-pitch-mode)

(use-package bufler)
(add-to-list 'evil-emacs-state-modes 'bufler-mode)
(add-to-list 'evil-emacs-state-modes 'bufler-tabs-mode)
(add-to-list 'evil-emacs-state-modes 'bufler-list-mode)

(use-package buffer-flip
  :config
  (setq buffer-flip-skip-patterns
        '("^\\*helm\\b"
          "^\\*swiper\\*$")))

(defun semacs/generate-scratch-buffer ()
  "Create and switch to a temporary scratch buffer with a random
       name."
  (interactive)
  (switch-to-buffer (make-temp-name "scratch-")))

(use-package vdiff
  :config
  ; This binds commands under the prefix when vdiff is active.
  (define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map))

(defun semacs/window-split()
(interactive)
;;(evil-window-split)
(switch-to-buffer-other-window "*test*")
(dired "~/")
    )

(defun nuke-all-buffers ()
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))

(global-set-key (kbd "C-x K") 'nuke-all-buffers)

(setq enable-recursive-minibuffers t)                                       ;Allow for minibuffer-ception. Sometimes we need another minibuffer command while we're in the minibuffer.
(setq echo-keystrokes 0.02)                                                 ;    Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any feedback after typing is better UX than no feedback at all.
(setq resize-mini-windows 'grow-only)                                       ;Expand the minibuffer to fit multi-line text displayed in the echo-area. This doesn't look too great with direnv, however...
(fset #'yes-or-no-p #'y-or-n-p)                                                   ;Typing yes/no is obnoxious when y/n will do
(setq minibuffer-prompt-properties
  '(read-only t intangible t cursor-intangible t face minibuffer-prompt))   ;Try really hard to keep the cursor from getting stuck in the read-only prompt portion of the minibuffer.
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(minibuffer-depth-indicate-mode)
(savehist-mode 1)                                                            ;Save minibuffer history
(setq-default history-length 1000)

(use-package which-key
  :init
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-max-display-columns 6
        which-key-min-display-lines 4
        which-key-side-window-slot -10)
  (setq which-key-max-description-length 27)
  (setq which-key-allow-evil-operators t)
  (setq which-key-show-operator-state-maps nil)
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

(use-package prescient
  :config
  ;; Remember usage statistics across Emacs sessions.
  (prescient-persist-mode +1)
  ;; The default settings seem a little forgetful to me. Let's try
  ;; this out.
  (setq prescient-history-length 1000))

(blink-cursor-mode -1)

(setq blink-matching-paren nil)

(setq x-stretch-cursor nil)

(setq save-place-file
      (expand-file-name "saveplace" semacs-cache-dir))
(if (version<= emacs-version "25.1")
    (progn
      (setq-default save-place t)
      (require 'saveplace))
  (save-place-mode 1))

(setq hscroll-margin 2)
(setq      hscroll-step 1)
      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically recentered.
(setq      scroll-conservatively 101)
(setq      scroll-margin 0)
(setq      scroll-preserve-screen-position t)
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
(setq      auto-window-vscroll nil)
      ;; mouse
(setq      mouse-wheel-scroll-amount '(2 ((shift) . hscroll)))
(setq      mouse-wheel-scroll-amount-horizontal 2)

(setq confirm-kill-emacs 'y-or-n-p)

(use-package restart-emacs)

;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'my-create-non-existent-directory)

(use-package dashboard
   :config
    (dashboard-setup-startup-hook)

    (setq dashboard-startup-banner (expand-file-name "apperance-dashboard-lion.png" semacs-emacs-dir))
    (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
    ;(setq dashboard-startup-banner 2)
;; TEST
    (defun dashboard-insert-custom (list-size)
    (insert "Custom text"))
    (add-to-list 'dashboard-item-generators  '(custom . dashboard-insert-custom))
    (add-to-list 'dashboard-items '(custom) t)

;; Edits
    (setq dashboard-banner-logo-title "Welcome to Semacs!")
    (setq dashboard-init-info "Your pain is the breaking of the shell that encloses your understanding.")
    (setq dashboard-show-shortcuts nil)
    ;(setq dashboard-startup-banner 'official)
    ;; Content is not centered by default. To center, set
    (setq dashboard-center-content t)
    (setq dashboard-set-init-info t)
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-navigator t)
    (setq dashboard-set-file-icons t)
    (setq dashboard-items '((recents . 10)
                            (projects . 5)
                            (bookmarks  . 5)
                            (agenda    . 5))))

(provide 'apperance-dashboard)
;;;Dashboard in Emacsclient
;;;This setting ensures that emacsclient always opens on dashboard rather than scratch.
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(use-package projectile
  :config
  (projectile-global-mode 1)
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package helm-projectile
  :config
  (helm-projectile-on))

(use-package deft
      :after org
      :bind
    ("C-c n d" . deft)
      :custom
      (deft-recursive t)
      (deft-use-filter-string-for-filename t)
      (deft-default-extension "org")
      (deft-directory "~/Org/"))

(setq dired-guess-shell-alist-user '(("" "xdg-open")))
;(define-key dired-mode-map "c" 'find-file)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(use-package dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired+)

(use-package dired-open
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :config
  (dired-hide-dotfiles-mode)
  (define-key dired-mode-map "." 'dired-hide-dotfiles-mode))
;;; Kill all dired buffers
    (defun kill-all-dired-buffers ()
      "Kill all dired buffers."
      (interactive)
      (save-excursion
        (let ((count 0))
          (dolist (buffer (buffer-list))
            (set-buffer buffer)
            (when (equal major-mode 'dired-mode)
              (setq count (1+ count))
              (kill-buffer buffer)))
          (message "Killed %i dired buffer(s)." count))))

;;; Kill dired buffer
    (defun kill-dired-buffers ()
	 (interactive)
	 (mapc (lambda (buffer)
           (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
             (kill-buffer buffer)))
         (buffer-list)))

(setq dired-find-file-other-buffer t)

;;; dired subtree
(use-package dired-subtree
:config
(setq dired-subtree-use-backgrounds nil)
  (let ((map dired-mode-map))
    (define-key map (kbd "<tab>") #'dired-subtree-toggle)
    (define-key map (kbd "<C-tab>") #'dired-subtree-cycle)
    (define-key map (kbd "<backtab>") #'dired-subtree-remove))
) ; S-TAB

(use-package neotree
  :init
  (require 'neotree)
 :config
   (setq neo-autorefresh nil)
   (setq neo-window-width 45)
   (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
   (setq neo-smart-open t))
(provide 'init-neotree)
(global-set-key [f8] 'neotree)

(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "x") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
(evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
(evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
(evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "h") 'neotree-hidden-file-togglf)

(defun seamacs/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun semacs/visit-config.org ()
  (interactive)
  (find-file (expand-file-name "config.org" user-emacs-directory)))

(defun semacs/visit-init.el ()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(use-package recentf
  ;; Keep track of recently opened files
  :commands recentf-open-files
  :config
  (defun semacs--recent-file-truename (file)
    (if (or (file-remote-p file nil t)
            (not (file-remote-p file)))
        (file-truename file)
      file))
    (setq recentf-filename-handlers
        '(;; Text properties inflate the size of recentf's files, and there is
          ;; no purpose in persisting them, so we strip them out.
          substring-no-properties
          ;; Resolve symlinks of local files. Otherwise we get duplicate
          ;; entries opening symlinks.
          semacs--recent-file-truename
          ;; Replace $HOME with ~, which is more portable, and reduces how much
          ;; horizontal space the recentf listing uses to list recent files.
          abbreviate-file-name)
        recentf-save-file (concat semacs-cache-dir "recentf")
        recentf-auto-cleanup 'never
        recentf-max-menu-items 0
        recentf-max-saved-items 200)
)

(straight-use-package
 '(openwith :type git :host github :repo "jpkotta/openwith")
:config
    (when (require 'openwith nil 'noerror)
      (setq openwith-associations
            (list
             (list (openwith-make-extension-regexp
                    '("mpg" "mpeg" "mp3" "mp4"
                      "avi" "wmv" "wav" "mov" "flv"
                      "ogm" "ogg" "mkv"))
                   "vlc"
                   '(file))
             (list (openwith-make-extension-regexp
                    '("xbm" "pbm" "pgm" "ppm" "pnm"
                      "png" "gif" "bmp" "tif" "jpeg" "jpg"))
                   "geeqie"
                   '(file))
             (list (openwith-make-extension-regexp
                    '("doc" "xls" "ppt" "odt" "ods" "odg" "odp"))
                   "libreoffice"
                   '(file))
             '("\\.lyx" "lyx" (file))
             '("\\.chm" "kchmviewer" (file))
             (list (openwith-make-extension-regexp
                    '("pdf" "ps" "ps.gz" "dvi"))
                   "okular"
                   '(file))
             ))
      (openwith-mode 1))
 )
;; https://github.com/jpkotta/openwith/tree/1dc89670822966fab6e656f6519fdd7f01e8301a

(use-package dmenu
  :bind
    ("s-SPC" . 'dmenu))

(setq dmenu-cfg " | dmenu -i -l 20 -p .")

(defun dmenu-ag ()
  (interactive)

  (setq my_shell_output (shell-command-to-string (concat "ag . " dmenu-cfg)))
  (setq splitted (split-string my_shell_output ":"))

  (when (> (length splitted) 1)
    (find-file (car splitted))
    (goto-line (string-to-number (nth 1 splitted)))))

(defun dmenu-ag-in-file ()
  (interactive)

  (setq my_command (concat "ag . " buffer-file-name dmenu-cfg))
  (setq my_shell_output (shell-command-to-string my_command))
  (setq splitted (split-string my_shell_output ":"))

  (when (> (length splitted) 1)
    (goto-line (string-to-number (car splitted)))))

(defun dmenu-find-file (&optional dir)
  (interactive)

  (setq command (concat "ls -a" dmenu-cfg))
  (setq output (shell-command-to-string command))

  (when (> (length output) 1)
    (find-file (substring output 0 -1))))

(defun dmenu-switch-buffer (&optional dir)
  (interactive)

  (setq command (concat (format "%s" (counsel-ibuffer)) dmenu-cfg))
  (setq splitted (split-string my_shell_output ":"))

  (print command))

;; Favor spaces over tabs. Pls dun h8, but I think spaces (and 4 of them) is a
;; more consistent default than 8-space tabs. It can be changed on a per-mode
;; basis anyway (and is, where tabs are the canonical style, like go-mode).
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Only indent the line when at BOL or in a line's indentation. Anywhere else,
;; insert literal indentation.
(setq-default tab-always-indent nil)

;; Make `tabify' and `untabify' only affect indentation. Not tabs/spaces in the
;; middle of a line.
(setq tabify-regexp "^\t* [ \t]+")


;; This was a widespread practice in the days of typewriters. I actually prefer
;; it when writing prose with monospace fonts, but it is obsolete otherwise.
(setq sentence-end-double-space nil)

;; The POSIX standard defines a line is "a sequence of zero or more non-newline
;; characters followed by a terminating newline", so files should end in a
;; newline. Windows doesn't respect this (because it's Windows), but we should,
;; since programmers' tools tend to be POSIX compliant (and no big deal if not).
(setq require-final-newline t)

;; Default to soft line-wrapping in text modes. It is more sensibile for text
;; modes, even if hard wrapping is more performant.
(add-hook 'text-mode-hook #'visual-line-mode)

(delete-selection-mode t)                           ;Delete Selection mode lets you treat an Emacs region much like a typical text selection outside of Emacs.

(use-package smartparens
  :init
  (smartparens-global-mode)
  :config
  (add-hook 'text-mode-hook 'smartparens-mode)
  (add-hook 'prog-mode-hook 'smartparens-mode))

(use-package avy
  :defer 4
  :commands (avy-goto-word-1))

(defun semacs/move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun semacs/move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "M-<down>") 'semacs/move-line-down)
(global-set-key (kbd "M-<up>") 'semacs/move-line-up)

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

(use-package tex-smart-umlauts)

(use-package ws-butler
  ;; a less intrusive `delete-trailing-whitespaces' on save
  :config
  ;; ws-butler normally preserves whitespace in the buffer (but strips it from
  ;; the written file). While sometimes convenient, this behavior is not
  ;; intuitive. To the average user it looks like whitespace cleanup is failing,
  ;; which causes folks to redundantly install their own.
  (setq ws-butler-keep-whitespace-before-point nil))

(use-package ripgrep)

(setq undo-limit 80000000                          ;Raise undo-limit to 80Mb
      evil-want-fine-undo t )                      ;Be more granular

(use-package undo-fu
  :after undo-tree
  :config
  (add-hook 'prog-mode-hook 'undo-tree-mode)
  (add-hook 'text-mode-hook 'undo-tree-mode)
  (global-undo-tree-mode -1)
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo))

(use-package undo-fu-session
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (setq undo-fu-session-directory (concat semacs-cache-dir "undofu/"))
  (add-hook 'org-mode-hook
  (lambda ()
    (undo-fu-session-mode)))
)

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (unbind-key "M-_" undo-tree-map))

(setq kill-do-not-save-duplicates t)

(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(setq x-select-enable-clipboard t)

(setq x-select-enable-primary t)

(unless IS-WINDOWS
  (setq selection-coding-system 'utf-8)) ; with sugar on top

(defun saa/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode) default-directory (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(use-package clipmon
  :init
  (add-to-list 'after-init-hook 'clipmon-mode-start))

(use-package browse-kill-ring
  :config
  (setq browse-kill-ring-highlight-inserted-item t
        browse-kill-ring-highlight-current-entry nil
        browse-kill-ring-show-preview t)
)

(use-package unicode-escape
  :init
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
(set-language-environment "UTF-8")

(use-package clipetty
:hook (after-init . global-clipetty-mode))

(defun my/consult-yank-or-yank-pop (&optional arg)
  "Call `consult-yank'. If called after a yank, call `yank-pop' instead."
  (interactive "*p")
  (if (eq last-command 'yank)
      (yank-pop arg)
    (consult-yank)))

(use-package company
:config
(setq company-selection-wrap-around t
      company-idle-delay 0.5
      company-minimum-prefix-length 2
      company-show-numbers t)
(add-hook 'evil-normal-state-entry-hook #'company-abort) ;; make aborting less annoying.
;===========================================
(company-tng-configure-default)
(global-company-mode t)
)


(use-package company-php
  :defer
  :after company)

(setq save-abbrevs 'silent)        ;; save abbrevs when files are saved
(setq-default abbrev-mode t)
(load "~/Coding/elisp/Semacs/snippets/semacs-abbrev.el")

(use-package yasnippet
  :init
  (yas-global-mode)
  :config
    ;(setq yas-snippet-dirs (concat semacs-emacs-dir "snippets"))
    ;(setq yas-snippet-dirs (expand-file-name "snippets" semacs-emacs-dir))
    (setq yas-snippet-dirs '("~/Coding/elisp/Semacs/snippets"))
    (yas-global-mode 1)
    (use-package yasnippet-snippets)
    (yas-reload-all))


(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand)

(defun seamacs/today (&optional arg)
"Insert today's date.

A prefix ARG specifies how many days to move;
negative means previous day.

If region selected, parse region as today's date pivot."
  (interactive "P")
  (let ((date (if (use-region-p)
                  (ts-parse (buffer-substring-no-properties (region-beginning) (region-end)))
                (ts-now)))
        (arg (or arg 0)))
    (if (use-region-p)
        (delete-region (region-beginning) (region-end)))
    (insert (ts-format "%A, %B %e, %Y" (ts-adjust 'day arg date)))))

(defun semacs/insert-filename-as-heading ()
  "Take current filename (word separated by dash) as heading."
  (interactive)
  (insert
   (capitalize
    (replace-regexp-in-string "-" " " (file-name-sans-extension (buffer-name))))))

(use-package academic-phrases)

(use-package lorem-ipsum)

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

(use-package flycheck-inline
  :config (global-flycheck-inline-mode))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

;  :init
 ; (global-flycheck-mode t))

  ;(add-hook 'text-mode-hook 'flyspell-mode)
  ;(add-hook 'prog-mode-hook 'flyspell-prog-mode)

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

(setq create-lockfiles nil                                            ; Don't generate backups or lockfiles. While auto-save maintains a copy so long
      make-backup-files nil
      ;; But in case the user does enable it, some sensible defaults:
      version-control t     ; number each backup file
      backup-by-copying t   ; instead of renaming current file (clobbers links)
      delete-old-versions t ; clean up after itself
      kept-old-versions 5
      kept-new-versions 5
      backup-directory-alist (list (cons "." (concat semacs-cache-dir "backup/")))
      tramp-backup-directory-alist backup-directory-alist)

(setq vc-follow-symlinks t)                                           ; Resolve symlinks when opening files, so that any operations are conducted from the file's true directory (like `find-file').
;(setq find-file-visit-trueme t)   ; Does not work with org-roam
(setq find-file-suppress-same-file-warnings t)                        ; Disable the warning "X and Y are the same file". It's fine to ignore this warning as it will redirect you to the existing buffer anyway.

(setq auto-save-default t
      ;; Don't auto-disable auto-save after deleting big chunks. This defeats
      ;; the purpose of a failsafe. This adds the risk of losing the data we
      ;; just deleted, but I believe that's VCS's jurisdiction, not ours.
      auto-save-include-big-deletions t
      ;; Keep it out of `semacs-emacs-dir' or the local directory.
      auto-save-list-file-prefix (concat semacs-cache-dir "autosave/")
      tramp-auto-save-directory  (concat semacs-cache-dir "tramp-autosave/")
      auto-save-file-name-transforms
      (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  ;; Prefix tramp autosaves to prevent conflicts with local ones
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            (list ".*" auto-save-list-file-prefix t)))

(use-package super-save
  :config
  (super-save-mode +1))

;; add integration with ace-window

(add-to-list 'super-save-triggers 'ace-window)
;; save on find-file
(add-to-list 'super-save-hook-triggers 'find-file-hook)

(use-package magit
  :commands magit-file-delete
  :init
  (setq magit-auto-revert-mode nil)  ; we do this ourselves further down
  ;; Must be set early to prevent ~/.emacs.d/transient from being created
  :config

  (setq transient-default-level 5
        magit-diff-refine-hunk t ; show granular diffs in selected hunk
        ;; Don't autosave repo buffers. This is too magical, and saving can
        ;; trigger a bunch of unwanted side-effects, like save hooks and
        ;; formatters. Trust the user to know what they're doing.
        magit-save-repository-buffers nil
        ;; Don't display parent/related refs in commit buffers; they are rarely
        ;; helpful and only add to runtime costs.
        magit-revision-insert-related-refs nil)
)

(use-package diff-hl
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(defun semacs/markdown-convert-buffer-to-org ()
   "Convert the current buffer's content from markdown to orgmode format and save it with the current buffer's file name but with .org extension."
   (interactive)
   (shell-command-on-region (point-min) (point-max)
                            (format "pandoc -f markdown -t org -o %s"
                                    (concat (file-name-sans-extension (buffer-file-name)) ".org"))))

(use-package pandoc-mode)

(use-package ox-pandoc
  :after org)
;; default options for all output formats
;(setq org-pandoc-options '((standalone . _)))
;; cancel above settings only for 'docx' format
;(setq org-pandoc-options-for-docx '((standalone . nil)))
;; special settings for beamer-pdf and latex-pdf exporters
;(setq org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex")))
;(setq org-pandoc-options-for-latex-pdf '((pdf-engine . "pdflatex")))
;; special extensions for markdown_github output
;(setq org-pandoc-format-extensions '(markdown_github+pipe_tables+raw_html))

;; open docx files in default application (ie msword)
(setq org-file-apps
      '(("\\.docx\\'" . default)
        ("\\.mm\\'" . default)
        ("\\.x?html?\\'" . default)
        ("\\.pdf\\'" . default)
        (auto-mode . emacs)))

(require 'tramp)
(setq tramp-verbose 10)
(setq tramp-debug-buffer t)

(defun connect-remote ()
  "Connect to Ubuntu Server"
  (interactive)
  (dired "/sftp:ubuntu@192.168.188.39:~"))

(use-package define-word
  :defer 5
)

(use-package google-translate
  :custom
  (google-translate-backend-method 'curl)
  :config
   (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130)))

(use-package better-shell
    :bind (("C-;" . better-shell-shell)
           ("C-'" . better-shell-remote-open)))

(setq exec-path (append exec-path '("/usr/local/bin")))

(use-package exec-path-from-shell)

(setq counsel-spotify-client-id "ce31becb1af94921907671e4bfa7f558")
(setq counsel-spotify-client-secret "9433b011b7094b2b8c4eb0255b8249e3")
(use-package counsel-spotify
:config
(require 'counsel-spotify)
)

(defun counsel-spotify-format-play-linux (uri)
  "Tell Spotify app to play the given URI."
  (format "OpenUri 'string:%s'" uri))

(setq sql-sqlite-program "/usr/bin/sqlite3")
(setq calibredb-program "/opt/calibre/bin/calibredb")
(use-package calibredb
  :defer t
  :init
  ;;(autoload 'calibredb "calibredb")
  :config
    (setq calibredb-root-dir "~/Calibre Library")
    (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
    (setq calibredb-library-alist '(("~/Calibre Library")))
)

(defvar calibredb-show-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" #'calibredb-entry-dispatch)
    (define-key map "v" #'calibredb-find-file)
    (define-key map "V" #'calibredb-find-file-other-frame)
    (define-key map "O" #'calibredb-open-file-with-default-tool)
    (define-key map "s" #'calibredb-set-metadata-dispatch)
    (define-key map "e" #'calibredb-export-dispatch)
    (define-key map "q" #'calibredb-entry-quit)
    (define-key map "y" #'calibredb-yank-dispatch)
    (define-key map "," #'calibredb-quick-look)
    (define-key map "." #'calibredb-open-dired)
    (define-key map "\M-/" #'calibredb-rga)
    (define-key map "\M-t" #'calibredb-set-metadata--tags)
    (define-key map "\M-a" #'calibredb-set-metadata--author_sort)
    (define-key map "\M-A" #'calibredb-set-metadata--authors)
    (define-key map "\M-T" #'calibredb-set-metadata--title)
    (define-key map "\M-c" #'calibredb-set-metadata--comments)
    map)
  "Keymap for `calibredb-show-mode'.")

(defvar calibredb-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-3] #'calibredb-search-mouse)
    (define-key map (kbd "<RET>") #'calibredb-find-file)
    (define-key map "?" #'calibredb-dispatch)
    (define-key map "a" #'calibredb-add)
    (define-key map "A" #'calibredb-add-dir)
    (define-key map "c" #'calibredb-clone)
    (define-key map "d" #'calibredb-remove)
    (define-key map "D" #'calibredb-remove-marked-items)
    (define-key map "h" #'calibredb-find-counsel)
    (define-key map "j" #'calibredb-next-entry)
    (define-key map "k" #'calibredb-previous-entry)
    (define-key map "l" #'calibredb-virtual-library-list)
    (define-key map "L" #'calibredb-library-list)
    (define-key map "n" #'calibredb-virtual-library-next)
    (define-key map "N" #'calibredb-library-next)
    (define-key map "p" #'calibredb-virtual-library-previous)
    (define-key map "P" #'calibredb-library-previous)
    (define-key map "s" #'calibredb-set-metadata-dispatch)
    (define-key map "S" #'calibredb-switch-library)
    (define-key map "v" #'calibredb-find-file)
    (define-key map "V" #'calibredb-find-file-other-frame)
    (define-key map "o" #'calibredb-view)
    (define-key map "O" #'calibredb-open-file-with-default-tool)
    (define-key map "," #'calibredb-quick-look)
    (define-key map "." #'calibredb-open-dired)
    (define-key map "y" #'calibredb-yank-dispatch)
    (define-key map "b" #'calibredb-catalog-bib-dispatch)
    (define-key map "e" #'calibredb-export-dispatch)
    (define-key map "r" #'calibredb-search-refresh-and-clear-filter)
    (define-key map "R" #'calibredb-search-clear-filter)
    (define-key map "q" #'calibredb-search-quit)
    (define-key map "m" #'calibredb-mark-and-forward)
    (define-key map "f" #'calibredb-toggle-favorite-at-point)
    (define-key map "x" #'calibredb-toggle-archive-at-point)
    (define-key map "h" #'calibredb-toggle-highlight-at-point)
    (define-key map "u" #'calibredb-unmark-and-forward)
    (define-key map "i" #'calibredb-edit-annotation)
    (define-key map (kbd "<DEL>") #'calibredb-unmark-and-backward)
    (define-key map (kbd "<backtab>") #'calibredb-toggle-view)
    (define-key map (kbd "TAB") #'calibredb-toggle-view-at-point)
    (define-key map "\M-n" #'calibredb-show-next-entry)
    (define-key map "\M-p" #'calibredb-show-previous-entry)
    (define-key map "/" #'calibredb-search-live-filter)
    (define-key map "\M-t" #'calibredb-set-metadata--tags)
    (define-key map "\M-a" #'calibredb-set-metadata--author_sort)
    (define-key map "\M-A" #'calibredb-set-metadata--authors)
    (define-key map "\M-T" #'calibredb-set-metadata--title)
    (define-key map "\M-c" #'calibredb-set-metadata--comments)
    map)
  "Keymap for `calibredb-search-mode'.")

(use-package elfeed
  :config
    (setq elfeed-feeds'(
        "https://emacsformacosx.com/atom/daily"
        "https://www.reddit.com/r/emacs.rss"))
    ;(setq elfeed-use-curl nil)
    ;(add-to-list 'evil-emacs-state-modes 'elfeed-search-mode)
    ;(add-to-list 'evil-emacs-state-modes 'elfeed-show-mode)
     (setq elfeed-curl-program-name "/usr/bin/curl"))

(use-package nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(eval-after-load "webjump"
'(add-to-list 'webjump-sites
              '("Urban Dictionary" .
                [simple-query
                 "www.urbandictionary.com"
                 "http://www.urbandictionary.com/define.php?term="
                 ""])))
;-------------------------------------------------
(eval-after-load "webjump"
'(add-to-list 'webjump-sites
              '("Melpa" .
                [simple-query
                 "https://melpa.org"
                 "http://melpa.org/#/?q="
                 ""])))
;-------------------------------------------------
(eval-after-load "webjump"
'(add-to-list 'webjump-sites
              '("Youtube" .
                [simple-query
                 "https://youtube.com"
                 "https://youtube.com/results?search_query="
                 ""])))

(use-package amread-mode
  :defer t
  :commands (amread-mode))

(use-package helpful)

(use-package crux)

(use-package ts)

(use-package web-mode
  :config
    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
  )

;(defun my-web-mode-hook ()
;  "Hooks for Web mode."
;  (setq web-mode-markup-indent-offset 2)
;  (setq web-mode-code-indent-offset 2)
;  (setq web-mode-css-indent-offset 2)
;)
;(add-hook 'web-mode-hook  'my-web-mode-hook)
;(setq tab-width 2)
;
;(setq web-mode-enable-current-column-highlight t)
;(setq web-mode-enable-current-element-highlight t)

(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
(defun web-mode-init-hook ()
  "Hooks for Web mode.  Adjust indent."
  (setq web-mode-markup-indent-offset 4))

(add-hook 'web-mode-hook  'web-mode-init-hook)

(use-package js2-mode)

(use-package js-comint
  :config
(add-hook 'rjsx-mode-hook
          (lambda ()
            (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
            (local-set-key (kbd "C-M-x") 'js-send-last-sexp-and-go)
            (local-set-key (kbd "C-c b") 'js-send-buffer)
            (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go)
            (local-set-key (kbd "C-c l") 'js-load-file-and-go))
    ))

(use-package emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

(use-package elpy
    :init
    (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
    :bind (:map elpy-mode-map
	      ("<M-left>" . nil)
	      ("<M-right>" . nil)
;	      ("<M-S-left>" . elpy-nav-indent-shift-left)
;	      ("<M-S-right>" . elpy-nav-indent-shift-right)
	      ("M-." . elpy-goto-definition)
	      ("M-," . pop-tag-mark))
    :config
    (setq elpy-rpc-backend "jedi"))

(use-package python
  :mode ("\\.py" . python-mode)
  :config
  (setq python-indent-offset 4)
  (elpy-enable))

(use-package pyenv-mode
  :init
  ;(add-to-list 'exec-path "~/.pyenv/shims")
  ;(setenv "~/.pyenv/versions/")
  (setq exec-path (append exec-path '("~/.pyenv/bin")))
  :config
  (pyenv-mode)
  :bind
  ("C-x p e" . pyenv-activate-current-project))

(defun pyenv-activate-current-project ()
  "Automatically activates pyenv version if .python-version file exists."
  (interactive)
  (let ((python-version-directory (locate-dominating-file (buffer-file-name) ".python-version")))
    (if python-version-directory
        (let* ((pyenv-version-path (f-expand ".python-version" python-version-directory))
               (pyenv-current-version (s-trim (f-read-text pyenv-version-path 'utf-8))))
          (pyenv-mode-set pyenv-current-version)
          (message (concat "Setting virtualenv to " pyenv-current-version))))))

(use-package php-mode)

(use-package haskell-mode)
(require 'haskell-mode-autoloads)

(package-initialize)
(when (eq system-type 'gnu/linux)
(setq user-org-directory (concat (getenv "HOME") "/Org/"))
(setq org_notes (expand-file-name "zettelkasten/" user-org-directory))
(setq org_agenda (expand-file-name "zettelkasten/task.org" user-org-directory))
(setq org_bib (expand-file-name "zettelkasten/bibnotes.org" user-org-directory))
(setq org_export (expand-file-name "zettelkasten/org-export/" user-org-directory))
(setq zot_bib (expand-file-name "zettelkasten/MeineBibliothek.bib" user-org-directory))
)

;Quickly visit Zettelkasten
(defun saa/visit-roam-zettelkasten ()
  (interactive)
  (find-file "~/Org/zettelkasten/index.org"))

;Org-Settings
(use-package org
  :init
  (progn
    (setq org-directory user-org-directory)
    (setq org-log-done 'time)
    (setq org-completion-use-ido t))
  :config
    (setq org-return-follows-link t)              ;Open Links with =RET=
    (setq org-drawers (quote ("PROPERTIES" "CLOCKTABLE" "LOGBOOK" "CLOCK")))
    (setq org-startup-folded 'fold)
    (setq org-cycle-separator-lines 0)             ;Structure the collapsed view
    (setq org-support-shift-select t)
    (setq org-agenda-start-with-log-mode t)
    (setq org-log-into-drawer t)
    (setq org-ellipsis " ")
    (setq org-startup-indented t)                  ;Indent according to section
    (setq org-startup-with-inline-images t)        ;Display images in-buffer by default
    (setq org-link-frame-setup
      '((file . find-file)))                       ;Open Org-Link in same buffer
    (setq org-src-tab-acts-natively t)
    (setq org-src-preserve-indentation t)
    (setq org-todo-keywords
      '((sequence "TODO" "FEEDBACK" "VERIFY" "CANCELLED" "NOTINUSE" "|" "DONE" "DELEGATED")))
    (setq org-todo-keyword-faces
      '(("TODO" :foreground "#7c7c75" :weight normal :underline t)
        ("FEEDBACK" :foreground "#9f7efe" :weight normal :underline t)
        ("VERIFY" :foreground "#0098dd" :weight normal :underline t)
        ("DONE" :foreground "#50a14f" :weight normal :underline t)
        ("NOTINUSE" :foreground "#50a14f" :weight normal :underline t)
        ("DELEGATED" :foreground "#B5123E" :weight normal :underline t)
        ("CANCELLED" :foreground "#ff6480" :weight normal :underline t)))
    (setq org-priority-faces
      '((65 :foreground "#e45649")
        (66 :foreground "#da8548")
        (67 :foreground "#0098dd"))))

;; Font size
(dolist (face '((org-level-1 . 1.1)
                (org-level-2 . 1.0)
                (org-level-3 . 1.0)
                (org-level-4 . 1.0)
                (org-level-5 . 1.0)
                (org-level-6 . 1.0)
                (org-level-7 . 1.0)
                (org-level-8 . 1.0)))
        (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

;;
(custom-theme-set-faces
  'user
  '(org-block ((t (:inherit fixed-pitch))))
  '(org-code ((t (:inherit (shadow fixed-pitch)))))
  '(org-document-info ((t (:foreground "dark orange"))))
  '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
  '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
  '(org-link ((t (:foreground "royal blue" :underline t))))
  '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  '(org-property-value ((t (:inherit fixed-pitch))) t)
  '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
  '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 1.0))))
  '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

(use-package org-appear
  :defer t
  :straight (org-appear :type git :host github :repo "awth13/org-appear")
  :hook (org-mode . org-appear-mode)
  :init
  (setq org-appear-autoemphasis  t)
  ;; (setq org-appear-autolinks t)
  (setq org-appear-autosubmarkers t))

(setq org-confirm-babel-evaluate 'nil)             ; Don't ask before executing

(org-babel-do-load-languages
'org-babel-load-languages
'(
 (R . t)
   (dot . t)
   (js . t)
   ;(php . t)
   (emacs-lisp . t)
   (python . t)
   (latex . t)
   (shell . t)
  ))

;Org-Babel-Block-Setting
(setq org-src-fontify-natively t
      org-src-window-setup 'current-window         ;edit in current window
      org-src-strip-leading-and-trailing-blank-lines t
      org-src-preserve-indentation t               ; do not put two spaces on the left
      org-src-tab-acts-natively t
      org-src-preserve-indentation t)

(use-package org-rich-yank
  :defer 6)

(defun org-export-output-file-name-modified (orig-fun extension &optional subtreep pub-dir)
  (unless pub-dir
    (setq pub-dir "~/Org/org-export")
    (unless (file-directory-p pub-dir)
      (make-directory pub-dir)))
  (apply orig-fun extension subtreep pub-dir nil))
(advice-add 'org-export-output-file-name :around #'org-export-output-file-name-modified)

(straight-use-package
 '(org-exsty :type git :host github :repo "seansen/org-exsty")
;:config (setq org-exsty-directory "FUCK")
 )

(use-package org-fancy-priorities
  :after org
  :hook
    (org-mode . org-fancy-priorities-mode)
  :config
    (setq org-fancy-priorities-list '("\u2764" "\u2708" "\u2706" "test" "\u270e")))

(use-package helm-org-rifle)

(use-package org-noter)

;(use-package org-pdftools
;  :hook (org-mode . org-pdftools-setup-link))

(use-package org-ref
  :after calibredb
  :config
  ;; org-ref
    (setq reftex-default-bibliography '("~/Calibre/catalog.bib"))

    (setq org-ref-bibliography-notes "~/Org/notes.org"
          org-ref-default-bibliography '("~/Calibre/catalog.bib")
          org-ref-pdf-directory "~/books/")
  ;; bibtex
    (setq bibtex-completion-bibliography "~/Calibre/catalog.bib"
          bibtex-completion-library-path "~/books"
          bibtex-completion-notes-path "~/Org/Helm-Bibtex-Notes")
;; KP
    (setq org-ref-completion-library 'org-ref-ivy-cite
          org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
          org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
          org-ref-notes-function 'orb-edit-notes)

(require 'org-ref)

;; calibredb
    (setq calibredb-ref-default-bibliography (concat (file-name-as-directory calibredb-root-dir) "catalog.bib"))
    ;(setq calibredb-ref-default-bibliography "~/Org/MeineBibliothek.bib")
    (add-to-list 'org-ref-default-bibliography calibredb-ref-default-bibliography)
    (setq org-ref-get-pdf-filename-function 'org-ref-get-mendeley-filename)

)

(use-package org-roam
  :hook
    (after-init . org-roam-mode)
  :custom
    (org-roam-db-location "~/Org/zettelkasten/org-roam.db")
    (org-roam-directory "~/Org/zettelkasten")
    (benchmark 1 '(org-roam-db-build-cache))
    (org-roam-buffer-position 'right)
    (org-roam-buffer-width 0.3)
    (org-roam-index-file "index.org")
    (org-roam-verbose nil)  ; https://youtu.be/fn4jIlFwuLU
    (org-roam-buffer-no-delete-other-windows t) ; make org-roam buffer sticky
    (org-roam-title-change-hook '(org-roam--update-links-on-title-change))
    (org-roam-db-update-method 'immediate)
    (setq org-roam-dailies-directory "daily/")
    (setq org-roam-dailies-directory "~/Org/daily/")
    (org-roam-buffer-window-parameters '((no-delete-other-windows . t))
    (org-roam-completion-everywhere t)
    (org-roam-completion-system)
    (cond ((featurep! :completion helm) 'helm)
          ((featurep! :completion ivy) 'ivy)
          ((featurep! :completion ido) 'ido)
           ('default)))
    (defvar +org-roam-open-buffer-on-find-file t
      "If non-nil, open the org-roam buffer when opening an org roam file.")
;; Normally, the org-roam buffer doesn't open until you explicitly call
;; `org-roam'. If `+org-roam-open-buffer-on-find-file' is non-nil, the
;; org-roam buffer will be opened for you when you use `org-roam-find-file'
;; (but not `find-file', to limit the scope of this behavior).
   (add-hook 'find-file-hook
     (defun +org-roam-open-buffer-maybe-h ()
     (and +org-roam-open-buffer-on-find-file
       (memq 'org-roam-buffer--update-maybe post-command-hook)
       (not (window-parameter nil 'window-side)) ; don't proc for popups
       (not (eq 'visible (org-roam-buffer--visibility)))
       (with-current-buffer (window-buffer)
       (org-roam-buffer--get-create)))))


)

(setq org-roam-capture-templates
  '(
   ("d" "default" plain (function org-roam--capture-get-point)
    "%?"
      :file-name
        "%(format-time-string \"%Y%m%d%H%M%S-${slug}\" (current-time) t)"
      :head

"#+TITLE: ${title}
#+AUTHOR: Sean Averhoff
#+CREATED: %U
#+LAST_MODIFIED: %U
#+STARTUP: OVERVIEW
#+ROAM_ALIAS:
#+ROAM_TAGS:
#+SETUPFILE: ~/Semacs/straight/repos/org-exsty/styles-html/retro_dark.theme \n"
      :unnarrowed t)

   ("b" "blog" plain (function org-roam--capture-get-point)
    "%?"
      :file-name  "%(format-time-string \"%Y%m%d%H%M%S-${slug}\" (current-time) t)"
      :head
"#+TITLE: ${title}
#+AUTHOR: Sean Averhoff
#+CREATED: %U
#+LAST_MODIFIED: %U
#+STARTUP: OVERVIEW
#+ROAM_ALIAS:
#+ROAM_TAGS:
#+SETUPFILE: ~/Semacs/straight/repos/org-exsty/styles-html/retro_dark.theme \n"
      :unnarrowed t)

  ("p" "private" plain (function org-roam--capture-get-point)
   "%?"
      :file-name
        "private-${slug}"
      :head
"#+TITLE: ${title}
#+Author: ${author-or-editor}
#+CREATED: %U
#+LAST_MODIFIED: %U
#+STARTUP: OVERVIEW"
      :unnarrowed t)))

(use-package company-org-roam
  :after org-roam
  :config
  (push 'company-org-roam company-backends))

;;** TODO Org-Roam-Bibtex
(use-package org-roam-bibtex
  :load-path "~/Org/zettelkasten" ;Modify with your own path
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
    (add-hook 'after-init-hook #'org-roam-bibtex-mode)
    (define-key org-roam-bibtex-mode-map (kbd "C-c n a") #'orb-note-actions)
)

(use-package org-roam-server
  :after org-roam
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-label-truncate t
        org-roam-server-label-truncate-length 60
        org-roam-server-label-wrap-length 20)
  (defun org-roam-server-open ()
    "Ensure the server is active, then open the roam graph."
    (interactive)
    (org-roam-server-mode 1)
    (browse-url-xdg-open (format "http://localhost:%d" org-roam-server-port))))

(use-package nroam
  :straight '(nroam :host github
                    :branch "master"
                    :repo "NicolasPetton/nroam")
  :config
  (add-hook 'org-mode-hook #'nroam-setup-maybe))

;;Block-ref and block-embed are two concepts in Roam Research.
;;With the two functionalities, users can easily refer or include one
;;block in another block or page. Roam-block has implemented these in emacs.
;;[[https://github.com/Kinneyzhang/roam-block][GitHub - Roam-Block]]
;(use-package roam-block
 ; :after org-roam
  ;:load-path "path/to/roam-block/"
  ;  :hook (after-init . roam-block-mode)
 ;   :init (setq roam-block-home '("~/roam-block/")
 ;               roam-block-ref-highlight t
   ;             roam-block-embed-highlight t)
  ;  :bind
  ;  (:map roam-block-mode-map
     ;     (("C-c b r s" . roam-block-ref-store)
     ;      ("C-c b r i" . roam-block-ref-insert)
     ;      ("C-c b r d" . roam-block-ref-delete)
     ;      ("C-c b r t" . roam-block-ref-highlight-toggle)
     ;      ("C-c b e s" . roam-block-embed-store)
     ;      ("C-c b e i" . roam-block-embed-insert)
     ;      ("C-c b e t" . roam-block-embed-highlight-toggle)
     ;      ("C-c b d" . roam-block-delete-block))))

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

;(defun transform-square-brackets-to-round-ones(string-to-transform)
;  "Transforms [ into ( and ] into ), other chars left unchanged."
;  (concat (mapcar #'(lambda (c) (if (equal c ?[) ?\( (if (equal c ?]) ?\) c))) ;string-to-transform)))

(setq org-capture-templates
      '(("t" "Aufgabe in tasks.org" entry (file+headline "~/projects/org/tasks.org" "Inbox")
         "* TODO %?")
        ("w" "Waiting For Reply (Mail)" entry (file+headline "~/projects/org/tasks.org" "Inbox")
         "* WAITING Antwort auf %a")
        ("m" "Aufgabe aus Mail" entry (file+headline "~/projects/org/tasks.org" "Inbox")
         "* TODO %? , Link: %a")
        ("z" "Zeiteintrag in tasks.org" entry (file+headline "~/projects/org/tasks.org" "Inbox")
         "* ZKTO %? \n  %i" :clock-in t :clock-resume t)
        ("c" "Contacts" entry (file "~/projects/org/contacts.org")
         "* %(org-contacts-template-name) \n :PROPERTIES: %(org-contacts-template-email) \n :BIRTHDAY: \n :END:")
        ("j" "Journal" entry (file+datetree "~/projects/org/journal.org")
         "* %?\nEntered on %U\n  %i")
        ("p" "password" entry (file "~/projects/org/passwords.gpg")
         "* %^{Title}\n  %^{PASSWORD}p %^{USERNAME}p")))

;; Automatische Anpassung des Links zu einer E-Mail.
;; Alle Mails werden im Folder 'Archive' gespeichert.
(add-hook 'org-capture-prepare-finalize-hook 'hs/search)
(defun hs/search ()
  (interactive)
  (goto-char 1)
  (replace-string "INBOX" "Archive"))

(org-reload)

(use-package org-cliplink)

(use-package ivy
  :config
  (ivy-mode 1)
  ;;(setq search-default-mode #'char-fold-to-regexp)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-initial-inputs-alist nil)
  (setq enable-recursive-minibuffers t))

;Counsel is just ivy, but with a custom tailored UI for each specific situation.
(use-package counsel)

;Remember Commands that where uses
(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode 1)
  ;; Remember candidate frequencies across sessions
  (prescient-persist-mode 1)
  ;How many chosen candidates will be remembered
  (setq prescient-history-length 150)
  ;A multiplier applied to each frequent candidate each selection (default: 0.997)
  (setq prescient-frequency-decay 0.997)
  ;Threshold used for forgotten a command that isn't used frequently anymore (default: 0.5)
  (setq prescient-frequency-threshold 0.5)
  (setq prescient-filter-method '(literal regexp))
)

(use-package ivy-rich
  :after counsel
  :custom
  (ivy-virtual-abbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer)
  (ivy-rich-mode 1)) ;; this gets us descriptions in M-x.

(use-package helm)

(use-package swiper)

;(add-hook 'text-mode-hook 'smartparens-mode)
(add-hook 'text-mode-hook 'tex-smart-umlauts-mode)

(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'visual-line-mode)
(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;(add-hook 'prog-mode-hook 'smartparens-mode)
(add-hook 'prog-mode-hook 'yas-global-mode)
(add-hook 'prog-mode-hook 'tex-smart-umlauts-mode)

(defun autoformat ()
  "Automatically format current buffer."
  (interactive)

  (if (derived-mode-p 'clojure-mode)
      (autoformat-clojure-function)
    (let ((eslint-path (concat (projectile-project-root)
                               ".eslintrc.yml"))) ; could be .json or .yml
      (autoformat-with
       (cond ((derived-mode-p 'web-mode) 'autoformat-html-command)
             ((derived-mode-p 'css-mode) 'autoformat-css-command)
             ((derived-mode-p 'json-mode) 'autoformat-json-command)
             ((derived-mode-p 'sass-mode) 'autoformat-sass-command)
             ((derived-mode-p 'yaml-mode) 'autoformat-yaml-command)
             ((derived-mode-p 'enh-ruby-mode) 'autoformat-ruby-command)
             ;; JS projects with eslint config
             ((and (file-exists-p eslint-path)
                   (derived-mode-p 'js2-mode))
              'autoformat-prettier-eslint-command)
             ((derived-mode-p 'js2-mode) 'autoformat-javascript-command))))))

(defun autoformat-with (strategy)
  "Automatically format current buffer using STRATEGY."
  (let ((p (point))
        (s (window-start)))
    ;; Remember the current position
    (save-mark-and-excursion
      ;; Call prettier-eslint binary with the contents of the current
      ;; buffer
      (shell-command-on-region
       (point-min) (point-max)
       (funcall strategy)
       ;; Write into a temporary buffer
       (get-buffer-create "*Temp autoformat buffer*")
       ;; Replace the current buffer with the output of
       ;; the =autoformat strategy= output
       t
       ;; If the =autoformat strategy= returns an error, show it in a
       ;; separate error buffer
       (get-buffer-create "*replace-errors*")
       ;; Automatically show error buffer
       t))
    ;; Return to the previous point and scrolling position (the point
    ;; was lost, because the whole buffer got replaced.
    (set-window-start (selected-window) s)
    (goto-char p)))

(defun autoformat-clojure-function ()
  "Cider function to format Clojure buffer."
  (indent-buffer)
  ;; (cider-format-buffer)
  )

(defun autoformat-ruby-command ()
  "CLI tool to format Ruby."
  "prettier --parser ruby")

(defun autoformat-javascript-command ()
  "CLI tool to format Javascript."
  "prettier --parser babel")

(defun autoformat-html-command ()
  "CLI tool to format HTML."
  "prettier --parser html")

(defun autoformat-css-command ()
  "CLI tool to format CSS."
  "prettier --parser css")

(defun autoformat-sass-command ()
  "CLI tool to format SASS."
  "prettier --parser sass")

(defun autoformat-json-command ()
  "CLI tool to format JSON."
  "prettier --parser json")

(defun autoformat-yaml-command ()
  "CLI tool to format YAML."
  "prettier --parser yaml")

(defun autoformat-prettier-eslint-command ()
  "CLI tool to format Javascript with .eslintrc.json configuration."
  (concat "npx prettier-eslint  --stdin --eslint-config-path="
          ;; Hand over the path of the current projec
          (concat
           (projectile-project-root)
           ".eslintrc.yml")
          " --stdin-filepath="
          (buffer-file-name)
          " --parser babel"))

(setq ok-autoformat-modes (list 'web-mode
                'css-mode
                'json-mode
                'clojure-mode
                'sass-mode
                'enh-ruby-mode
                'yaml-mode
                'js2-mode
                'rjsx-mode))

(dolist (mode ok-autoformat-modes)
  ;(evil-leader/set-key-for-mode mode "f" 'autoformat)
)

(window-divider-mode)

(setenv "SHELL" "/bin/zsh")
  (setq tramp-shell-prompt-pattern "\\(?:^\\|
\\)[^]#$%>\n]*#?[]#$%>\ue0b0] *\\(\\[[0-9;]*[a-zA-Z] *\\)*")

(setq remote-org-directory "/ssh:ubuntu@192.168.188.39:OrgFiles")

(defcustom remote-org-directory "~/OrgFiles"
  "Location of remove OrgFile directory, should you have one."
  :type 'string
  :group 'paf)
(defun paf/open-remote-org-directory ()
  (interactive)
  (find-file remote-org-directory))

(evil-define-state test
  "Test state."
  :tag "<t>"
  :message "TEDT"
  (message (if (evil-test-state-p)
               "Enabling test state."
               "Disabling test state."))
)

;(setq-default prettify-symbols-alist
;                '(("#+BEGIN_SRC"     . "")
;                  ("#+END_SRC"       . ""))

(defun mark-entire-line ()
  "mark the whole line from the indent to the end"
  (interactive)
  (beginning-of-line-text)
  (set-mark-command nil)
  (end-of-line))
