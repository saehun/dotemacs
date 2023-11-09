;;; init-markdown.el --- Completion with markdown -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; WAITING: markdown-mode sets tags-table-list globally, breaks tags-completion-at-point-function
;; TODO Default sort order should place [a-z] before punctuation
(require 'markdown-mode)
(require 'face-remap)
(require 'projectile)
(require 'xref)


(custom-set-variables
  '(markdown-command "/usr/local/bin/pandoc"))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-hook 'markdown-mode-hook #'visual-line-mode)
(add-hook 'markdown-mode-hook
  (lambda () (add-hook 'before-save-hook #'markdown-auto-insert-date)))

(defun markdown-jump ()
  "Push xref mark and jump to link."
  (interactive)
  (xref--push-markers (current-buffer) (point))
  (markdown-follow-link-at-point))


;;----------------------------------------------------------------------------
;; Markdown insert date
;;----------------------------------------------------------------------------
(defun markdown-auto-insert-date ()
  "Insert date at markdown buffer."
  (interactive)
  (if (and
        (eq major-mode 'markdown-mode)
        (string-equal "TIL" (projectile-project-name)))
    (if (not (time-comment-p "date"))
      (save-excursion
        (goto-char 0)
        (insert (format "<!-- date: %s, %s -->\n" (format-time-string "%Y-%m-%dT%T") (format-time-string "%Y-%m-%dT%T"))))
      (save-excursion
        (goto-char 0)
        (while (re-search-forward "<!-- date: \\([0-9:T-]+\\), [0-9:T-]+ -->" nil t)
          (replace-match (format "<!-- date: \\1, %s -->" (format-time-string "%Y-%m-%dT%T")))))
      )))

(defun time-comment-p (key)
  "Find comment for given KEY."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (re-search-forward (format "<!-- %s: [0-9:T-]+, [0-9:T-]+ -->" key) nil t))))

(defun markdown-config ()
  "Markdown Initialization."
  (interactive)
  (setq buffer-face-mode-face '(:family "Palatino"))
  (setq markdown-fontify-code-blocks-natively 't)
  (set-face-font 'markdown-code-face "-*-Monaco-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")
  (buffer-face-mode))

;;----------------------------------------------------------------------------
;; Main configuration
;;----------------------------------------------------------------------------



(provide 'init-markdown)
;;; init-markdown.el ends here
