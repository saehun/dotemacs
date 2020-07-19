(require 'dash)

(when (maybe-require-package 'find-file-in-project)
  ;; well, I'm not interested in concatenated BIG js file or file in dist/
  
  (setq ffip-find-options "-not -size +64k -not -iwholename '*/dist/*' -not -iwholename '*/build/*' -not -iwholename '*/coverage/*'")
  (setq ffip-ignore-filenames '("*.bmp" "*.jpg" "*.png" "package-lock.json" "yarn.lock"))

  (defun ffip-setup ()
      (interactive)
  (setq-local ffip-find-options "-not -size +64k -not -iwholename '*/dist/*'")
  ;; for this project, I'm only interested certain types of files
  (setq-local ffip-ignore-filenames '("*.bmp" "*.jpg" "*.png" "package-lock.json" "yarn.lock"))
  (--each
    (--map
      (concat "*/" it)
      '("coverage" "build" ".next" "elpa*" "auto-save-list" "emacs-backup" "dist" ".terraform"))
    (add-to-list 'ffip-prune-patterns it))))

(provide 'init-ffip)
