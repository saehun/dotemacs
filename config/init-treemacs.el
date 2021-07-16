;;; init-treemacs.el --- Work with Treemacs configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Treemacs

(when (maybe-require-package 'treemacs)
  (maybe-require-package 'treemacs-projectile)

  (setq treemacs-width 50)
  (setq treemacs--width-is-locked nil)

  (treemacs-resize-icons 18))
  
  

(provide 'init-treemacs)
;;; init-treemacs.el ends here
