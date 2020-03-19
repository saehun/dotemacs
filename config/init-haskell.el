;;; init-haskell.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'haskell-mode)

  (use-package dante
    :ensure t
    :after haskell-mode
    :commands 'dante-mode
    :init
    (add-hook 'haskell-mode-hook 'dante-mode))

  (when (maybe-require-package 'flycheck-haskell)
    (add-hook 'haskell-mode-hook #'flycheck-haskell-setup))

    (require 'dante)
    (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint) 'append)
  )

(provide 'init-haskell)
;;; init-haskell.el ends here
