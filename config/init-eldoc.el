;;; init-eldoc.el --- Completion with company -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'eldoc-box)
  (add-hook 'tide-mode-hook (lambda () (eldoc-box-hover-at-point-mode +1)))
  (add-hook 'go-mode-hook (lambda () (eldoc-box-hover-at-point-mode +1)))
  (add-hook 'rustic-mode-hook (lambda () (eldoc-box-hover-at-point-mode +1)))
  (add-hook 'emacs-lisp-mode-hook (lambda () (eldoc-box-hover-at-point-mode +1))))

(provide 'init-eldoc)
;;; init-eldoc.el ends here
