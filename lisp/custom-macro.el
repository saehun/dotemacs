;; Global macro
(defun semicolon-macro ()
  (interactive)
  (end-of-line)
  (insert ";"))
(global-set-key (kbd "s-;") 'semicolon-macro)

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))
(global-set-key (kbd "C-s-r")  'revert-buffer-no-confirm)


(provide 'custom-macro)
