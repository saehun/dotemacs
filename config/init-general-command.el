;;; init-general-command.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun goto-test ()
  "Goto test or create one."
  (interactive)
  (cond
    ((or 
       (string-equal "ts" (file-name-extension buffer-file-name))
       (string-equal "tsx" (file-name-extension buffer-file-name))
       )
      (node/new-test))
    (t (message "Cannot go to the test file. Unknown file buffer."))))


;;----------------------------------------------------------------------------
;; Build
;;----------------------------------------------------------------------------
(defun build ()
  "Univerally build any project."
  (interactive)
  (let* ((project-root (projectile-project-root)))
    (cond
      ((active-mode-p 'tide-mode)
        (let ((package-root-dir (find-directory-has-file-until-reach project-root "package.json")))
          (run-build--package-json package-root-dir project-root)))
      (t (message "Universal build is not supported for current buffer. 🥺")))))

(defun run-build--package-json (package-root-dir project-root-dir)
  (let ((lockfile-exist-p
          (lambda (lockfile)
            (or
              (file-exists-p (concat package-root-dir "/" lockfile))
              (file-exists-p (concat project-root-dir "/" lockfile))))))
    (cond
      ((funcall lockfile-exist-p "package-lock.json") (message "npm run build: 🔥") (async-shell-command "npm run build"))
      ((funcall lockfile-exist-p "pnpm-lock.yaml") (message "pnpm build: 🔥") (async-shell-command "pnpm build"))
      ((funcall lockfile-exist-p "yarn.lock") (message "yarn build: 🔥") (async-shell-command "yarn build"))
      (t (message "Missing lockfile.")))))

;;----------------------------------------------------------------------------
;; Test
;;----------------------------------------------------------------------------
(defun test ()
  "Univerally test any project."
  (interactive)
  (let* ((project-root (projectile-project-root)))
    (cond
      ((active-mode-p 'tide-mode)
        (let ((package-root-dir (find-directory-has-file-until-reach project-root "package.json")))
          (run-test--package-json package-root-dir project-root)))
      (t (message "Universal test is not supported for current buffer. 🥺")))))

(defun run-test--package-json (package-root-dir project-root-dir)
  (let ((lockfile-exist-p
          (lambda (lockfile)
            (or
              (file-exists-p (concat package-root-dir "/" lockfile))
              (file-exists-p (concat project-root-dir "/" lockfile))))))
    (cond
      ((funcall lockfile-exist-p "package-lock.json") (message "npm run test: 🔥") (async-shell-command "npm run test"))
      ((funcall lockfile-exist-p "pnpm-lock.yaml") (message "pnpm test: 🔥") (async-shell-command "pnpm test"))
      ((funcall lockfile-exist-p "yarn.lock") (message "yarn test: 🔥") (async-shell-command "yarn test"))
      (t (message "Missing lockfile.")))))

;;----------------------------------------------------------------------------
;; Utils
;;----------------------------------------------------------------------------
(defun find-directory-has-file-until-reach (dir filename)
  "Find directory has file (FILENAME) until reach DIR."
  (cl-labels
    ((find-recursive (current-dir)
       (if (file-exists-p (concat current-dir "/" filename)) current-dir
         (if (string= current-dir dir) nil (find-recursive (parent-directory current-dir))))))
  (find-recursive default-directory)))

(defun parent-directory (dir)
  "Return parent directory of DIR."
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

(defun active-minor-mode-list ()
  "Give a message of which minor modes are enabled in the current buffer."
  (interactive)
  (let ((active-modes))
    (mapc
      (lambda (mode)
        (condition-case nil
          (if (and (symbolp mode) (symbol-value mode))
            (add-to-list 'active-modes mode))
          (error nil) ))
      minor-mode-list)
    active-modes))

(defun active-mode-p (mode)
  "Predicate whether MODE is active."
  (if (derived-mode-p mode) t
    (if (cl-member mode (active-minor-mode-list)) t
      nil)))

(provide 'init-general-command)

;; (defmacro iterate-alist (alist)
;;   `(cl-loop for (key . value) in ,alist
;;      collect (message "%s: %s" key value)))
;; 
;; (macroexpand (iterate-alist
;;                '(("package-lock.json" . "npm run build")
;;                  ("pnpm-lock.yaml" . "pnpm build")
;;                  ("yarn.lock" . "yarn build"))))
;; 
;; (defmacro if-node-package-manager-then-run (command-name package-manager-command-plist)
;;  `(defun ,command-name--package-json (package-root-dir project-root-dir)
;;     (let ((lockfile-exist-p
;;             (lambda (lockfile)
;;               (or
;;                 (file-exists-p (concat package-root-dir "/" lockfile))
;;                 (file-exists-p (concat project-root-dir "/" lockfile))))))
;;       (cond
;;         ((funcall lockfile-exist-p "package-lock.json") (message "npm run build: 🔥") (async-shell-command "npm run build"))
;;         ((funcall lockfile-exist-p "pnpm-lock.yaml") (message "pnpm build: 🔥") (async-shell-command "pnpm build"))
;;         ((funcall lockfile-exist-p "yarn.lock") (message "yarn build: 🔥") (async-shell-command "yarn build"))
;;         (t (message "Missing lockfile."))))))


;;; init-general-command.el ends here
