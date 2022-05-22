;;; init-dired.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dired-ranger
  :ensure t
  :bind (:map dired-mode-map
              ("W" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("Y" . dired-ranger-paste)))

(setq dired-dwim-target t)
(setq dired-listing-switches "-alog")

(use-package zoxide
  :ensure t)

(provide 'init-dired)
;;; init-dired.el ends here
