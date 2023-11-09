(require 'dash)
(require 'find-file-in-project)

;; well, I'm not interested in concatenated BIG js file or file in dist/

(setq ffip-use-rust-fd t)
(setq ffip-project-file '("package.json" ".git"))

;; (setq ffip-find-options "-not -size +64k -not -iwholename '*/dist/*' -not -iwholename '*/build/*' -not -iwholename '*/coverage/*'")
;; (setq ffip-ignore-filenames '("*.bmp" "*.jpg" "*.png" "package-lock.json" "yarn.lock"))

(defun ffip-setup ()
  (interactive)
  (setq-local ffip-find-options "-not -size +64k -not -iwholename '*/dist/*'")
  ;; for this project, I'm only interested certain types of files
  (setq-local ffip-ignore-filenames '("*.bmp" "*.jpg" "*.png" "package-lock.json" "yarn.lock"))
  (setq-local ffip-project-file '("package.json" ".git"))
  (--each
    (--map
      (concat "*/" it)
      '("coverage" "build" ".next" "elpa*" "auto-save-list" "emacs-backup" "dist" ".terraform"))
    (add-to-list 'ffip-prune-patterns it)))


(defun ffip-insert-relative-path ()
  "Find file/directory and copy its relative path into `kill-ring'.
  If FIND-DIRECTORY-P is t, copy the directory path.

  Set `ffip-find-relative-path-callback' to format the result,
    (setq ffip-find-relative-path-callback 'ffip-copy-reactjs-import)
    (setq ffip-find-relative-path-callback 'ffip-copy-org-file-link)"
  (interactive)
  (let* ((cands (ffip-project-search "" nil)))
    (cond
      ((> (length cands) 0)
        (ffip-completing-read
          (ffip-hint)
          cands
          `(lambda (file)
             ;; only one item in project files
             (setq
               file
               (file-relative-name
                 file
                 (if buffer-file-name
                   (file-name-directory buffer-file-name)
                   (expand-file-name default-directory))))
             (save-excursion
               (forward-char 1)
               (insert file)))))
      (t (message "Nothing found!")))))

(provide 'init-ffip)
