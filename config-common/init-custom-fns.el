;;; init-custom-fns.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'dash)

(defun get-package-name ()
  "Get project root directory."
  (interactive)
  (message (vc-root-dir)))


(defun go-up-general ()
  "Goto upper level. go to dired buffer when buffer is file"
  (interactive)
  (let ((initial (current-buffer)))
    (progn
      (if (eq major-mode 'dired-mode)
        (dired-up-directory)
        (dired (file-name-directory buffer-file-name)))
      (kill-buffer initial))))

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun counsel-rg-here ()
    "Like `counsel-rg' but always searches from the cwd, not project root."
    (interactive)
    (counsel-rg nil default-directory))

;; font sizes
(global-set-key (kbd "s-=")
                (lambda ()
                  (interactive)
                  (let ((old-face-attribute (face-attribute 'default :height)))
                    (set-face-attribute 'default nil :height (+ old-face-attribute 10)))))

(global-set-key (kbd "s--")
                (lambda ()
                  (interactive)
                  (let ((old-face-attribute (face-attribute 'default :height)))
                    (set-face-attribute 'default nil :height (- old-face-attribute 10)))))

(provide 'init-custom-fns)
;;; init-custom-fns.el ends here
