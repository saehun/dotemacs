(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(setq gc-cons-threshold (* 128 1024 1024))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))
(setq temporary-file-directory "~/.emacs.d/tmp")

(provide 'init-system)