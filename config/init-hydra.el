;;; init-terraform.el --- Work with Terraform configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Hydra

(when (maybe-require-package 'hydra)
  (bind-key
    "C-x g" 
    (defhydra hydra-zoom (:hint nil) 
      "zoom"
        ("g" text-scale-increase "in")
        ("l" text-scale-decrease "out"))))

(provide 'init-hydra)
;;; init-terraform.el ends here
