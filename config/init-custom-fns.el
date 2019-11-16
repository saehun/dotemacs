;;; init-custom-fns.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'dash)

(defun get-package-name ()
  "Get project root directory."
  (interactive)
  (message (vc-root-dir)))

(provide 'init-custom-fns)
;;; init-custom-fns.el ends here
