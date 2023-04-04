;;; markdown-daily-mode.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar foo-count 0
  "Number of foos inserted into the current buffer.")

(defun insert-foo ()
  "Hmm"
  (interactive)
  (setq foo-count (1+ foo-count))
  (insert (format "foo %s" foo-count)))

(define-minor-mode markdown-daily-mode
  "Get your foos in the right places."
  :lighter "daily"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c f") 'insert-foo)
            map)
  (make-local-variable 'foo-count))

(provide 'markdown-daily-mode)

;;; markdown-daily-mode.el ends here
