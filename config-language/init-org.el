;;; init-org.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; 
;;;; Requirements:
(require 'yasnippet)
(require 'org)


;;; Code:
(defun yas-org-very-safe-expand ()
  "Safe expand."
  (let ((yas-fallback-behavior 'return-nil)) (yas-expand)))

(add-hook 'org-mode-hook
          (lambda ()
            (add-to-list 'org-tab-first-hook 'yas-org-very-safe-expand)
            (define-key yas-keymap [tab] 'yas-next-field)))


(message "init-org.el")
(provide 'init-org)
;;; init-org.el ends here
