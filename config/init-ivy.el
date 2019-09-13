(when (maybe-require-package 'ivy)
  (add-hook 'after-init-hook 'ivy-mode)
  (setq ivy-height 30)
  (when (maybe-require-package 'counsel)
    (counsel-mode t))
  (maybe-require-package 'smex))

(provide 'init-ivy)
