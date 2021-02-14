;;; init-treemacs.el --- Work with Treemacs configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Treemacs

(when (maybe-require-package 'treemacs)
  (maybe-require-package 'treemacs-projectile)

  (treemacs-resize-icons 18))
  
  

(provide 'init-treemacs)
;;; init-terraform.el ends here
