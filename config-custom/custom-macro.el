;;; package --- custom-macro
;;; Commentary:
;;; Code:
(require 'projectile)

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

(defun open-daily ()
  "Open daily markdown document."
  (interactive)
  (let ((filename (concat (format-time-string "%Y-%m-%d") ".md")))
    (find-file (f-join "~" "repo" "github.com" "minidonut" "Daily" "Docs" filename))))

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

(defun dired-git-root ()
  "Open git root directory"
  (interactive)
  (dired (projectile-locate-dominating-file default-directory ".git")))

(defun kill-buffers-of-deleted-files ()
  "Kill all buffers associated with files that have been deleted."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (and (buffer-file-name buffer)      ; Check if buffer is associated with a file
               (not (file-exists-p (buffer-file-name buffer)))) ; Check if the file exists
      (kill-buffer buffer)                     ; Kill the buffer if file doesn't exist
      (message "Killed buffer: %s" (buffer-name buffer)))))

(provide 'custom-macro)
;;; custom-macro ends here
