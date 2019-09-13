;;----------------------------------------------------------------------------
;; recent file mode
;;----------------------------------------------------------------------------
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)


;;----------------------------------------------------------------------------
;; editorconfig
;;----------------------------------------------------------------------------
(when (maybe-require-package 'editorconfig)
  (add-hook 'prog-mode-hook 'editorconfig-mode))

(when (maybe-require-package 'dired-ranger)
  (define-key dired-mode-map (kbd "y") 'dired-ranger-copy)
  (define-key dired-mode-map (kbd "p") 'dired-ranger-paste))

(provide 'init-misc)
