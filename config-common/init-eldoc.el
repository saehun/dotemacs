;;; init-eldoc.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; 
;;;; Requirements:
(require 'eldoc-box)

;;; Code:
(dolist
  (language-mode
    '(
       emacs-lisp-mode-hook
       go-mode-hook
       rustic-mode-hook
       tide-mode-hook
       ))
  (add-hook language-mode (lambda () (eldoc-box-hover-at-point-mode +1))))

(message "init-eldoc.el")
(provide 'init-eldoc)
;;; init-eldoc.el ends here

