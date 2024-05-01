;;; init-language-etc.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; 
;;;; Requirements:
(require 'apheleia)

;;; Code:
(add-to-list 'auto-mode-alist '("\\.yaml$"    . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yml$"     . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("Dockerfile"  . dockerfile-ts-mode))
(add-hook 'yaml-ts-mode-hook (lambda () (apheleia-mode -1)))
(add-hook 'json-mode-hook    (lambda () (apheleia-mode -1)))

(message "init-language-etc.el")
(provide 'init-language-etc)
;;; init-language-etc.el ends here

