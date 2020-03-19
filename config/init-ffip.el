(require 'dash)

(when (maybe-require-package 'find-file-in-project)
  (defun ffip-setup ()
  (interactive)
    ;; well, I'm not interested in concatenated BIG js file or file in dist/
    (setq-local ffip-find-options "-not -size +64k -not -iwholename '*/dist/*'")
    ;; for this project, I'm only interested certain types of files
    (setq-local ffip-ignore-filenames '("*.bmp" "*.jpg" "*.png" "package-lock.json" "yarn.lock"))
    (--each
        (--map
         (concat "*/" it)
         '("coverage" "build" ".next" "elpa*" "auto-save-list" "emacs-backup" "dist" ".terraform"))
      (add-to-list 'ffip-prune-patterns it))))

(provide 'init-ffip)
