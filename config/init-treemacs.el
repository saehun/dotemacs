;;; init-treemacs.el --- Work with Treemacs configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Treemacs

(when (maybe-require-package 'treemacs)
  (require 'projectile)
  (maybe-require-package 'treemacs-projectile)

  (setq treemacs-width 65)
  (setq treemacs--width-is-locked nil)


  (defun treemacs-open-project-automatically ()
    "Open treemacs project automatically."
    (let* ((path (or (projectile-locate-dominating-file default-directory "package.json")
                  (or (projectile-root-bottom-up default-directory)
                    default-directory)))
            (name (car (last (s-split "/" path) 2))))
      (if (not (--first (string= name (treemacs-project->name it))
                (treemacs-workspace->projects (treemacs-current-workspace))))
        (treemacs-add-project-to-workspace path name))))

  (defun treemacs-smart ()
    "Open and close treemacs and automatically add project."
    (interactive)
      (pcase (treemacs-current-visibility)
      ('visible (delete-window (treemacs-get-local-window)))
      ('exists (progn (treemacs-open-project-automatically) (treemacs-select-window)))
      ('none   (progn (treemacs-open-project-automatically) (treemacs--init)))))

  (treemacs-resize-icons 18))
  
  

(provide 'init-treemacs)
;;; init-treemacs.el ends here
