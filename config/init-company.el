;;; init-company.el --- Completion with company -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; WAITING: haskell-mode sets tags-table-list globally, breaks tags-completion-at-point-function
;; TODO Default sort order should place [a-z] before punctuation

;; (add-to-list 'completion-styles 'initials t)

(when (maybe-require-package 'company)
  (add-hook 'after-init-hook 'global-company-mode)
  (after-load 'company
    ;; (dolist (backend '(company-eclim company-semantic))
      ;; (delq backend company-backends))
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 1)
    (setq company-selection-wrap-around t)
    ; Use tab key to cycle through suggestions.
    ; ('tng' means 'tab and go')
    ;; (company-tng-configure-default)
    (setq-default company-dabbrev-other-buffers 'all company-tooltip-align-annotations t))
  (when (maybe-require-package 'company-quickhelp)
    (add-hook 'after-init-hook 'company-quickhelp-mode)
    (setq company-quickhelp-delay nil)))

(provide 'init-company)
;;; init-company.el ends here
