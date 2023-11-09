;; javascript init

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))
(setq js2-mode-show-parse-errors nil js2-mode-show-strict-warnings nil)
(setq js-indent-level 2)


(provide 'init-javascript)
