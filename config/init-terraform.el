;;; init-terraform.el --- Work with Terraform configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Terraform

(when (maybe-require-package 'terraform-mode)
  (when (maybe-require-package 'company-terraform)
    (after-load 'terraform-mode
      (progn
        (terraform-format-on-save-mode)
        (company-terraform-init))
      )))

(provide 'init-terraform)
;;; init-terraform.el ends here
