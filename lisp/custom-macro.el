;;; package --- custom-macro
;;; Commentary:
;;; Code:
(defun semicolon-macro ()
  "Put semicolon at the end of the line."
  (interactive)
  (end-of-line)
  (insert ";"))
(global-set-key (kbd "s-;") 'semicolon-macro)

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))
(global-set-key (kbd "C-s-r")  'revert-buffer-no-confirm)


(defun open-todo ()
  "Open todo markdown file."
  (interactive)
  (find-file (f-join "~" "null" "todo.md")))


(provide 'custom-macro)
;;; custom-macro ends here
