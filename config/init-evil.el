(when (maybe-require-package 'evil)
  (add-hook 'after-init-hook 'evil-mode)

  (when (maybe-require-package 'evil-surround)
    (global-evil-surround-mode 1))

  (when (maybe-require-package 'ace-jump-mode)
    '(ace-jump-mode-enable-mark-sync))

  (when (maybe-require-package 'evil-textobj-anyblock))
)

(provide 'init-evil)