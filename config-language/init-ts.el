;;; init-ts.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; 
;;;; Requirements:
(require 'tide)
(require 'web-mode)
(require 'flycheck)
(require 'c-ts-mode)
(require 'company)
(require 'jest-test-mode)

;;;; Requirements Local:
(require 'init-ts-command)
(require 'init-ts-hydra)


;;; Code:
(add-to-list 'auto-mode-alist '("\\.js$"  . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cjs$" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.mjs$" . js-ts-mode))

(add-to-list 'auto-mode-alist '("\\.ts$"  . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cts$" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.mts$" . typescript-ts-mode))

(add-to-list 'auto-mode-alist '("\\.tsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

(custom-set-variables '(js-indent-level 2))


;;; Web mode:
(setq web-mode-enable-auto-quoting nil)

;;; Tide:
(defun setup-tide-mode ()
  "Setup tide mode."
  (interactive)
  (tide-setup)
  ;; (tide-setup-yarn2)
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
  ;; (setq comment-start       "/*"
  ;;       comment-end         "*/"
  ;;       comment-multi-line  t
  ;;       comment-padding     nil
  ;;       comment-style       'extra-line
  ;;       comment-continue    " * "
  ;;       comment-empty-lines t)
  ;; (c-ts-mode-toggle-comment-style)
  (company-mode +1))

(defun tide-fix-all ()
  "Apply code fix for all."
  (interactive)
  (tide-code-fix #'tide-apply-codefix-for-all-in-file))

(add-hook 'typescript-ts-mode-hook #'setup-tide-mode)
(add-hook 'tsx-ts-mode-hook        #'setup-tide-mode)
(add-hook 'js-ts-mode-hook         #'setup-tide-mode)
(add-hook 'web-mode-hook           #'setup-tide-mode)

;;; Flycheck:
(flycheck-add-mode 'typescript-tide 'web-mode)
(flycheck-add-mode 'javascript-tide 'web-mode)

(flycheck-add-next-checker 'tsx-tide '(warning . javascript-eslint) 'append)
(flycheck-add-next-checker 'typescript-tide '(warning . javascript-eslint) 'append)

;;; Jest:
(add-hook 'typescript-ts-mode-hook 'jest-test-mode)
(add-hook 'tsx-ts-mode-hook        'jest-test-mode)
(add-hook 'js-ts-mode-hook         'jest-test-mode)
(add-hook 'web-mode-hook           'jest-test-mode)

(message "init-ts.el")
(provide 'init-ts)
;;; init-ts.el ends here
