;;; init-restclient.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; 
;;;; Requirements:
(require 'restclient)
(require 'company)
(require 'company-restclient)
(require 'evil)

;;; Code:
(add-to-list 'auto-mode-alist '("\\.http$" . restclient-mode))
(add-to-list 'company-backends 'company-restclient)
;; (add-to-list 'restclient-mode-hook (lambda () (electric-indent-mode -1)))


(message "init-restclient.el")
(provide 'init-restclient)
;;; init-restclient.el ends here

