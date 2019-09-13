(require 'dash)

(when (maybe-require-package 'find-file-in-project)
  (global-set-key (kbd "s-p") 'find-file-in-project)
  (defun ffip-setup ()
  (interactive)
    ;; well, I'm not interested in concatenated BIG js file or file in dist/
    (setq-local ffip-find-options "-not -size +64k -not -iwholename '*/dist/*'")
    ;; for this project, I'm only interested certain types of files
    (setq-local ffip-ignore-filenames '("*.bmp" "*.jpg" "package-lock.json" "yarn.lock"))
      (let (s) (-each
      (--map (concat "*/") '("coverage" "build" "next" "elpa*" "auto-save-list" "emacs-backup" "dist"))
                 (add-to-list 'ffip-prune-patterns s)))))

(provide 'init-ffip)
