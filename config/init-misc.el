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

(provide 'init-misc)
