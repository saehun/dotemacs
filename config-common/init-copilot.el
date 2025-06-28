;;; init-copilot.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; 
;;;; Requirements:
(require 'copilot)
(require 'typescript-ts-mode)

(defun copilot-complete-or-accept ()
  "Complete or new start show completion."
  (interactive)
  (or (copilot-accept-completion)
      (copilot-complete)))

(defun setup-copilot-mode ()
  "Enable copilot mode."
  (copilot-mode +1))

;;; Code:
(add-to-list 'copilot-major-mode-alist '("typescript-ts" . "typescript"))
(add-to-list 'copilot-major-mode-alist '("tsx-ts" . "typescriptreact"))
(add-to-list 'copilot-major-mode-alist '("rustic" . "rust"))
(add-to-list 'copilot-major-mode-alist '("org" . "plaintext"))
(add-to-list 'copilot-major-mode-alist '("go-ts" . "go"))


(define-key copilot-mode-map (kbd "<C-up>") #'copilot-next-completion)
(define-key copilot-mode-map (kbd "<C-down>") #'copilot-previous-completion)
(define-key copilot-mode-map (kbd "<tab>") #'copilot-complete-or-accept)
(define-key copilot-mode-map (kbd "<C-s-right>") #'copilot-accept-completion-by-word)

;;; Enable copilot:
(add-hook 'tsx-ts-mode-hook        #'setup-copilot-mode)
(add-hook 'typescript-ts-mode-hook #'setup-copilot-mode)
(add-hook 'rustic-mode-hook        #'setup-copilot-mode)
(add-hook 'bash-ts-mode            #'setup-copilot-mode)
(add-hook 'go-ts-mode-hook         #'setup-copilot-mode)
;; (add-hook 'emacs-lisp-mode-hook    #'setup-copilot-mode)

(message "init-copilot.el")
(provide 'init-copilot)
;;; init-copilot.el ends here
