;;; package --- custom-macro
;;; Commentary:
;;; Code:
(defun semicolon-macro ()
  "Put semicolon at the end of the line."
  (interactive)
  (end-of-line)
  (insert ";"))
(global-set-key (kbd "s-;") 'semicolon-macro)

(defun comma-macro ()
  "Put semicolon at the end of the line."
  (interactive)
  (end-of-line)
  (insert ","))
(global-set-key (kbd "s-,") 'comma-macro)

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t t)
  (message "Buffer reverted ✨"))

(defun open-todo ()
  "Open todo markdown file."
  (interactive)
  (find-file (f-join "~" "null" "todo.md")))

(defun open-til ()
  "Open til repository."
  (interactive)
  (find-file (f-join "~" "repo" "github.com" "minidonut" "TIL" "Docs")))

(defun jest/copy-command-for-current-file ()
  "Copy jest command for currnet file."
  (interactive)
  (kill-new
    (string-join
      (list
        "npx"
        "jest"
        (file-relative-name
          (f-no-ext (buffer-file-name))
          (projectile-project-root)))
      " ")))

(defun prev-window ()
  "Go to previous window."
  (interactive)
  (other-window -1))

(provide 'custom-macro)
;;; custom-macro ends here
