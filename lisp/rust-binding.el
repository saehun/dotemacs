;;; package --- rust-binding
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'projectile)

(defun rust-ensure-in-rust-sourcefile ()
  "Ensure current path is under typescript project and is in sourcefile."
  (if (or (not (buffer-file-name)) (not (eq major-mode 'rustic-mode)))
    (error "Not in rust source file")))

(defun execute-cargo-command (command)
  "Execute COMMAND such as run, test, debug."
  (rust-ensure-in-rust-sourcefile)
  (-let (((_ __ project-directory relative-filepath) (rust-get-paths)))
    (call-process-shell-command
      (format "find-session %s %s"
        (replace-regexp-in-string "/$" "" project-directory)
        (format "\"%s %s\"" command relative-filepath)) nil nil nil)))

(defun rust-get-paths ()
  "Get node paths."
  (let*
    ((filename (file-name-nondirectory (buffer-file-name)))
      (file-directory default-directory)
      (project-directory (projectile-locate-dominating-file default-directory "Cargo.toml"))
      (relative-filepath (f-relative (concat file-directory filename) project-directory)))
    (mapcar (lambda (path) (replace-regexp-in-string "/$" "" path))
      (list filename file-directory project-directory relative-filepath))))

(defun rust/run-current-file ()
  "Run current typescript file with ts-node."
  (interactive)
  (execute-cargo-command "cargo run"))

(defun rust/test-current-file ()
  "Test current typescript file with jest."
  (interactive)
  (execute-cargo-command "cargo test"))

(provide 'rust-binding)
;;; rust-binding.el ends here
