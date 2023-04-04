;;; package --- init-typescript
;;; Commentary:
;;; Code:

(defun rome ()
  "Apply rome formatter."
  (let ((current (point)))
    (call-process-region
      (point-min) (point-max) "rome" t t nil
      "format"
      "--stdin-file-path"
      "--line-width"
      "100"
      "--quote-style"
      "single"
      "--trailing-comma"
      "all"
      "index.ts")
     (goto-char current)))

(when (maybe-require-package 'web-mode)
  (setq web-mode-enable-auto-quoting nil)
  (when (maybe-require-package 'tide)
    (defun setup-tide-mode ()
      (interactive)
      (tide-setup)
      (tide-setup-yarn2)
      (flycheck-mode +1)

      (setq flycheck-check-syntax-automatically '(save mode-enabled))
      (setq tide-completion-show-source t)
      (setq tide-completion-ignore-case nil)
      (setq tide-server-max-response-length 1024000)
      (setq tide-node-flags '("--max-old-space-size=8192"))
      (setq tide-user-preferences
        '(
           :includeCompletionsForModuleExports t
           :includeCompletionsWithInsertText t
           :allowTextChangesInNewFiles t
           :generateReturnInDocTemplate t
           :quotePreference "single"
           :importModuleSpecifierPreference "relative"
           :autoImportFileExcludePatterns ["**/node_modules/date-fns"]
         ))

      ;; (eldoc-mode +1)
      (setq comment-start       "/*"
            comment-end         "*/"
            comment-multi-line  t
            comment-padding     nil
            comment-style       'extra-line
            comment-continue    " * "
            comment-empty-lines t)

      (company-mode +1))

    (defun tide-fix-all ()
      "Apply code fix for all."
      (interactive)
      (tide-code-fix #'tide-apply-codefix-for-all-in-file))


    (require' projectile)
    (defun tide-setup-yarn2 ()
      "Yarn2."
      (let* ((project-root (projectile-project-root)))
        (if
          (or
            (cl-search "toss/frontend-libraries" project-root)
            (cl-search "toss/frontend-devops" project-root)
            (cl-search "toss/tuba-ui" project-root)
            (cl-search "toss/toss-frontend-tuba" project-root)
            (cl-search "toss/toss-frontend" project-root)
            (cl-search "toss/ca-bridge" project-root))
          (progn
            (setq-local tide-tsserver-executable
              (concat project-root ".yarn/sdks/typescript/bin/tsserver"))
            (setq-local flycheck-javascript-eslint-executable
              (concat project-root ".yarn/sdks/eslint/bin/eslint.js")))
        )))



    ;; formats the buffer before saving
    (require 'flycheck)
    (add-hook 'typescript-mode-hook #'setup-tide-mode)
    
    (add-to-list 'auto-mode-alist '("\\.mjs\\'" . js2-mode))
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
    (add-hook 'web-mode-hook
              (lambda ()
                (when (string-equal "tsx" (file-name-extension buffer-file-name))
                  (setup-tide-mode))))
    (add-hook 'web-mode-hook
              (lambda ()
                (when (string-equal "ts" (file-name-extension buffer-file-name))
                  (setup-tide-mode))))

    (add-hook 'after-init-hook #'global-prettier-mode)
    (add-hook 'js2-mode-hook #'setup-tide-mode)
    ;;(add-hook 'js2-mode-hook 'prettier-js-mode)
    ;;(add-hook 'web-mode-hook 'prettier-js-mode)


    ;;(add-hook 'web-mode-hook
      ;;(lambda () (add-hook 'before-save-hook 'rome nil t)))

    (defun tide-jump-back-and-kill ()
      (interactive)
      (progn
        (kill-this-buffer)
        (tide-jump-back)))

    (require 'tide)
    (flycheck-add-mode 'typescript-tide 'web-mode)
    (flycheck-add-mode 'javascript-tide 'web-mode)

    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-add-next-checker 'tsx-tide '(warning . javascript-eslint) 'append)
    (flycheck-add-next-checker 'typescript-tide '(warning . javascript-eslint) 'append)

    (use-package jest-test-mode
      :ensure t
      :commands jest-test-mode
      :hook (web-mode js2-mode)))
)

(provide 'init-typescript)
;;; init-typescript.el ends here

