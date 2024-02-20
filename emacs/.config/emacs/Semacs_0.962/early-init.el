;;; early-init.el -*- lexical-binding: t; -*-

;; Don't load package.el since we're using straight instead. 
(setq package-enable-at-startup nil)

;; Keep all the messages around:
(setq message-log-max t)

;; Ensure Semacs is running out of this file's directory
;(setq user-emacs-directory (file-name-directory load-file-name))


