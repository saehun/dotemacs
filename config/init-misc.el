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
