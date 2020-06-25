;; javascript init

(when (maybe-require-package 'rjsx-mode)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
  (setq js-indent-level 2))


(provide 'init-javascript)
