;;; init-treemacs.el --- Work with Treemacs configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Treemacs

(when (maybe-require-package 'treemacs)
  (require 'projectile)
  (require 'treemacs)
  (require 'treemacs-async)
  (maybe-require-package 'treemacs-projectile)

  (setq treemacs-width 65)
  (setq treemacs--width-is-locked nil)

  ;;(add-hook treemacs-mode-hook 'treemacs-hide-gitignored-files-mode)

  (treemacs-git-mode 'extended)
  (treemacs-hide-gitignored-files-mode t)

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
        ('exists  (treemacs-add-and-display-current-project-exclusively))
        ('none    (treemacs-add-and-display-current-project-exclusively))))

  (treemacs-resize-icons 18))
  
  

(provide 'init-treemacs)
;;; init-treemacs.el ends here
