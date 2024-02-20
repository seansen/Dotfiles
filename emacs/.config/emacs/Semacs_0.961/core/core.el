;;; core.el --- Loads afer the init file -*- lexical-binding: t; -*-

;;; Commentary:
;;; - Initialize internal state
;;; - A quieter startup
;;; - No littering
;;; - Optimization

;;; Code:

;;
;;; Initialize internal state

(defconst semacs-version "1.0.2"
  "Current version of Semacs.")

;; Enviroments
(defconst EMACS27+   (> emacs-major-version 26))
(defconst EMACS28+   (> emacs-major-version 27))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))

;; Unix tools look for HOME, but this is normally not defined on Windows.
(when (and IS-WINDOWS (null (getenv-internal "HOME")))
  (setenv "HOME" (getenv "USERPROFILE"))
  (setq abbreviated-home-dir nil))

;; Make UTF-8 the default coding system.

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)

;; The clipboard's on Windows could be in a wider encoding than utf-8.
(unless IS-WINDOWS
  (setq selection-coding-system 'utf-8)) ; with sugar on top

;;
;;; A quieter startup

;; Disable warnings from legacy advice system.
(setq ad-redefinition-action 'accept)

;; Get rid of "Info about Emacs message at startup, unless we're in a daemon session.
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; Measure startup-performace
(add-hook 'dashboard-after-initialize-hook
;(add-hook 'emacs-startup-hook
  (lambda ()
    (message "*** Semacs loaded in %s with %d garbage collections."
      (format "%.2f seconds"
        (float-time
        (time-subtract after-init-time before-init-time)))
      gcs-done)))

;; Reduce *Message* noise at startup.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)


;; Make startup faster by reducing the frequency of garbage collection.
(setq gc-cons-threshold 64000000)

;; Restore after startup.
(add-hook 'after-init-hook #'(lambda ()
  (setq gc-cons-threshold 800000)))

;;
;;; No littering

;; We avoid `no-littering' because it's a mote too opinionated for our needs.
(setq async-byte-compile-log-file  (concat semacs-etc-dir "async-bytecomp.log")
      custom-file                  (concat semacs-private-dir "custom-settings.el")
      desktop-dirname              (concat semacs-etc-dir "desktop")
    ;;bookmark-default-file        (concat semacs-cache-dir "bookmarks")
      desktop-base-file-name       "autosave"
      desktop-base-lock-name       "autosave-lock"
      pcache-directory             (concat semacs-cache-dir "pcache/")
      request-storage-directory    (concat semacs-cache-dir "request")
      shared-game-score-directory  (concat semacs-etc-dir "shared-game-score/")
      projectile-known-projects-file (concat semacs-cache-dir "projectile-bookmarks.eld")
      prescient-save-file          (concat semacs-cache-dir "prescient-save.el"))

;;
;;; Optimizations

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted, and
;; indicates misconfiguration (don't rely on case insensitivity for file names).
(setq auto-mode-case-fold nil)

;; Disable bidirectional text rendering for a modest performance boost. I've set
;; this to `nil' in the past, but the `bidi-display-reordering's docs say that
;; is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Disabling the BPA makes redisplay faster, but might produce incorrect display
;; reordering of bidirectional text with embedded parentheses and other bracket
;; characters whose 'paired-bracket' Unicode property is non-nil.
(setq bidi-inhibit-bpa t)  ; Emacs 27 only

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Emacs "updates" its ui more often than it needs to, so we slow it down
;; slightly from 0.5s:
(setq idle-update-delay 1.0)

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but we inhibit it there anyway. This increases memory
;; usage, however!
(setq inhibit-compacting-font-caches t)

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help with performance while scrolling.
(setq redisplay-skip-fontification-on-input t)

;; Performance on Windows is considerably worse than elsewhere. We'll need
;; everything we can get.
(when IS-WINDOWS
  (setq w32-get-true-file-attributes nil   ; decrease file IO workload
        w32-pipe-read-delay 0              ; faster ipc
        w32-pipe-buffer-size (* 64 1024))) ; read more at a time (was 4K)

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless IS-MAC   (setq command-line-ns-option-alist nil))
(unless IS-LINUX (setq command-line-x-option-alist nil))

(load "core-packages.el")
(load "core-keybinds.el")
(load "core-editor.el")
(load "core-ui.el")
(load "core-cli.el")
(load "core-modules.el")
(load "user-settings.el")
