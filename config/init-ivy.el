(when (maybe-require-package 'ivy)
  (add-hook 'after-init-hook 'ivy-mode))

(provide 'init-ivy)