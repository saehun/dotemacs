;;; init-yasnippet.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'yasnippet)
  (yas-global-mode 1)
  (setq yas-snippet-dirs
    '("~/.emacs.d/snippets"))

  (when (maybe-require-package 'ivy-yasnippet))
  (when (maybe-require-package 'string-inflection)
    (string-inflection-all-cycle)))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
