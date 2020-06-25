;;----------------------------------------------------------------------------
;; recent file mode
;;----------------------------------------------------------------------------
(recentf-mode 1)
(setq recentf-max-menu-items 1000)
(global-set-key "\C-x\ \C-r" 'counsel-recentf)


;;----------------------------------------------------------------------------
;; Miscellaneous
;;----------------------------------------------------------------------------
(electric-pair-mode 1)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)


;;----------------------------------------------------------------------------
;; editorconfig
;;----------------------------------------------------------------------------
(when (maybe-require-package 'editorconfig)
  (add-hook 'prog-mode-hook 'editorconfig-mode))

(maybe-require-package 'yaml-mode)

(when (maybe-require-package 'drag-stuff)
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

(when (maybe-require-package 'dired-ranger)
  (define-key dired-mode-map (kbd "y") 'dired-ranger-copy)
  (define-key dired-mode-map (kbd "p") 'dired-ranger-paste))


(maybe-require-package 'golden-ratio)

(provide 'init-misc)
