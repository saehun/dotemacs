;;; init-cl.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (and
        (maybe-require-package 'slime)
        (maybe-require-package 'slime-company))
  (setq inferior-lisp-program "sbcl")

  ;; company
  (slime-setup '(slime-fancy slime-company))
  (add-to-list 'slime-contribs 'slime-autodoc)
  (setq slime-company-after-completion 'slime-company-just-one-space)
  (eldoc-mode +1)
  (flycheck-mode +1))

(provide 'init-cl)
;;; init-cl.el ends here
