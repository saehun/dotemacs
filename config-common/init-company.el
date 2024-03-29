;;; init-company.el --- Completion with company -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; WAITING: haskell-mode sets tags-table-list globally, breaks tags-completion-at-point-function
;; TODO Default sort order should place [a-z] before punctuation

;; (add-to-list 'completion-styles 'initials t)

(require 'company-box)

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :hook (company-mode . company-box-mode)
  :defines company-dabbrev-downcase
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  (setq company-dabbrev-downcase nil)
  (setq-default company-dabbrev-other-buffers 'all company-tooltip-align-annotations t))

(custom-set-variables
 '(company-box-doc-delay 0.1))

(use-package company-quickhelp
  :after company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'after-init-hook 'company-quickhelp-mode)
  :config
  (setq company-quickhelp-delay nil))

(provide 'init-company)
;;; init-company.el ends here
