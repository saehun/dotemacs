;;; init-markdown.el --- Completion with markdown -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; WAITING: haskell-mode sets tags-table-list globally, breaks tags-completion-at-point-function
;; TODO Default sort order should place [a-z] before punctuation
(require 'face-remap)
(require 'markdown-mode)

(defun markdown-config ()
  "Markdown Initialization."
  (interactive)
  (setq buffer-face-mode-face '(:family "Palatino"))
  (setq markdown-fontify-code-blocks-natively 't)
  (set-face-font 'markdown-code-face "-*-Monaco-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")
  (buffer-face-mode))

(when (maybe-require-package 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
  (add-hook 'markdown-mode-hook 'markdown-config)
  )

(provide 'init-markdown)
;;; init-markdown.el ends here
