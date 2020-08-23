;;; init-company.el --- Completion with company -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; WAITING: haskell-mode sets tags-table-list globally, breaks tags-completion-at-point-function
;; TODO Default sort order should place [a-z] before punctuation

(add-to-list 'completion-styles 'initials t)

(when (maybe-require-package 'company)
  (add-hook 'after-init-hook 'global-company-mode)
  (after-load 'company
    ;; (dolist (backend '(company-eclim company-semantic))
      ;; (delq backend company-backends))
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 2)
    (setq company-selection-wrap-around t)
    ; Use tab key to cycle through suggestions.
    ; ('tng' means 'tab and go')
    ;; (company-tng-configure-default)
    (setq-default company-dabbrev-other-buffers 'all company-tooltip-align-annotations t))
  (when (maybe-require-package 'company-quickhelp)
    (add-hook 'after-init-hook 'company-quickhelp-mode)))


;; add emoji backends😃
(when (maybe-require-package 'company-emoji))
(require 'company-emoji)
(add-to-list 'company-backends 'company-emoji)

;; Suspend page-break-lines-mode while company menu is active
;; (see https://github.com/company-mode/company-mode/issues/416)
(after-load 'company
  (after-load 'page-break-lines
    (defvar-local sanityinc/page-break-lines-on-p nil)

    (defun sanityinc/page-break-lines-disable (&rest ignore)
      (when (setq sanityinc/page-break-lines-on-p (bound-and-true-p page-break-lines-mode))
        (page-break-lines-mode -1)))

    (defun sanityinc/page-break-lines-maybe-reenable (&rest ignore)
      (when sanityinc/page-break-lines-on-p
        (page-break-lines-mode 1)))

    (add-hook 'company-completion-started-hook 'sanityinc/page-break-lines-disable)
    (add-hook 'company-after-completion-hook 'sanityinc/page-break-lines-maybe-reenable)))

(provide 'init-company)
;;; init-company.el ends here
