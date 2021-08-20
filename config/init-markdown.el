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
  (custom-set-variables
    '(markdown-command "/usr/local/bin/pandoc"))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-hook 'markdown-mode-hook #'visual-line-mode))
  ;; (add-hook 'markdown-mode-hook 'markdown-config)

(defun markdown-jump ()
  "Push xref marks and jump to link."
  (interactive)
  (xref--push-markers)
  (markdown-follow-link-at-point))

(provide 'init-markdown)
;;; init-markdown.el ends here
