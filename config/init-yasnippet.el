;;----------------------------

(when (maybe-require-package 'yasnippet)
  (yas-global-mode 1)
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets")))

(provide 'init-yasnippet)
