;;; init-terraform.el --- Work with Terraform configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Magit

(when (maybe-require-package 'magit)
  (setq magit-display-buffer-function
    (lambda (buffer)
      (display-buffer buffer '(display-buffer-same-window))))

  (setq magit-git-global-arguments '("--no-pager" "-c" "core.preloadindex=true" "-c" "log.showSignature=false" "-c" "color.ui=false" "-c" "color.diff=false")))

(provide 'init-magit)
;;; init-magit.el ends here
