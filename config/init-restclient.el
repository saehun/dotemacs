;;; init-restclient.el --- Completion with company -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'restclient)
  (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))
  )

(provide 'init-restclient)
;;; init-restclient.el ends here
