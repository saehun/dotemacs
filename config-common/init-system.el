;;; init-system.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; 
;;;; Requirements:
(require 'package)
(require 'autorevert)
(require 'treesit-auto)
(require 'apheleia)
(require 'marginalia)

;;; Code:
(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter // 8GB
;;----------------------------------------------------------------------------
(setq gc-cons-threshold (* 128 1024 1024))
;;(setq gc-cons-threshold (* 8 1024 1024 1024))

;;----------------------------------------------------------------------------
;; Set path
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))
(setq temporary-file-directory "~/.emacs.d/tmp")
(setq default-directory "~/repo/")

;;----------------------------------------------------------------------------
;; Auto save and revert behaviors
;;----------------------------------------------------------------------------
(setq auto-save-default nil)
(setq backup-inhibited t)
(setq make-backup-files nil)
(setq ring-bell-function 'ignore)
(global-auto-revert-mode  1)
(global-treesit-auto-mode 1)
(apheleia-global-mode +1)
(marginalia-mode 1)

(customize-set-variable 'treesit-font-lock-level 4)  ; Better syntax highlighting
(customize-set-variable 'large-file-warning-threshold 100000000) ;; change to ~100 MB

;;----------------------------------------------------------------------------
;; Auto refresh dired, but be quiet about it
;;----------------------------------------------------------------------------
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;;----------------------------------------------------------------------------
;; Setup default input method
;;----------------------------------------------------------------------------
(setq default-input-method "korean-hangul")

;;----------------------------------------------------------------------------
;; Setup coding system
;;----------------------------------------------------------------------------
;; Use UTF-8 for all character encoding.
(set-default-coding-systems 'utf-8)     ; Default to utf-8 encoding
(prefer-coding-system       'utf-8)     ; Add utf-8 at the front for automatic detection.
(set-terminal-coding-system 'utf-8)     ; Set coding system of terminal output
(set-keyboard-coding-system 'utf-8)     ; Set coding system for keyboard input on TERMINAL
(set-language-environment "English")    ; Set up multilingual environment

;; ESC cancels all
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; Bind restart emacs
(global-set-key (kbd "s-<f5>") 'restart-emacs)
;; Rebind C-u
(global-set-key (kbd "C-M-u") 'universal-argument)
;; C-h to F1
(setq help-char (string-to-char "<f1>"))


;;----------------------------------------------------------------------------
;; Improve long-line performance
;;----------------------------------------------------------------------------
(setq bidi-inhibit-bpa t)
;; (global-so-long-mode 1)

(message "init-system.el")
(provide 'init-system)
;;; init-system.el ends here
