;;; init-dired.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'dired)

(setq dired-dwim-target t)
(setq dired-listing-switches "-alog")

(provide 'init-dired)
;;; init-dired.el ends here
