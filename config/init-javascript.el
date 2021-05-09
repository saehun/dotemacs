;; javascript init

(when (maybe-require-package 'js2-mode)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))
  (setq js-indent-level 2))


(provide 'init-javascript)
