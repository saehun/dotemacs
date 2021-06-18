(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter // 8GB
;;----------------------------------------------------------------------------
(setq gc-cons-threshold (* 8 1024 1024 1024))

;;----------------------------------------------------------------------------
;; Set path
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))
(setq temporary-file-directory "~/.emacs.d/tmp")
(setq default-directory "~/wd/")

;;----------------------------------------------------------------------------
;; Auto save and revert behaviors
;;----------------------------------------------------------------------------
(setq auto-save-default nil)
(setq backup-inhibited t)
(setq make-backup-files nil)
(setq ring-bell-function 'ignore)
(global-auto-revert-mode t)

;; turn on lexical binding
(setq lexical-binding t)

;;----------------------------------------------------------------------------
;; Auto refresh dired, but be quiet about it
;;----------------------------------------------------------------------------
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;;----------------------------------------------------------------------------
;; Improve long-line performance
;;----------------------------------------------------------------------------
(setq bidi-inhibit-bpa t)
;; (global-so-long-mode 1)

(provide 'init-system)
