;; typescript init

(when (maybe-require-package 'web-mode)
  (setq web-mode-enable-auto-quoting nil)
  (when (maybe-require-package 'tide)
    (defun setup-tide-mode ()
      (interactive)
      (tide-setup)
      (tide-setup-yarn2)
      (flycheck-mode +1)
      (setq flycheck-check-syntax-automatically '(save mode-enabled))
      ;; (setq tide-tsserver-executable "node_modules/typescript/bin/tsserver")
      (eldoc-mode +1)
      (tide-hl-identifier-mode +1)
      ;; 이거 키면 company 버퍼 열리는게 매우 느려진다
      ;; (setq tide-completion-detailed t)
      (setq comment-start       "/*"
            comment-end         "*/"
            comment-multi-line  t
            comment-padding     nil
            comment-style       'extra-line
            comment-continue    " * "
            comment-empty-lines t)


      (company-mode +1))


    ;; Yarn2 support
    (require' projectile)
    (defun tide-setup-yarn2 ()
      "Yarn2."
      (let* ((project-root (projectile-project-root)))
        (if
          (or
            (cl-search "toss/toss-frontend-libraries" project-root)
            (cl-search "toss/frontend-devops" project-root))
          (progn
            (setq-local tide-tsserver-executable
              (concat project-root ".yarn/sdks/typescript/bin/tsserver"))
            (setq-local flycheck-javascript-eslint-executable
              (concat project-root ".yarn/sdks/eslint/bin/eslint.js")))
        )))

    ;; formats the buffer before saving
    (require 'flycheck)
    (add-hook 'typescript-mode-hook #'setup-tide-mode)
    
    ;; 대신 prettier를 사용
    ;; (add-hook 'before-save-hook 'tide-format-before-save)

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

    (add-hook 'rjsx-mode-hook #'setup-tide-mode)


    (defun tide-jump-back-and-kill ()
      (interactive)
      (progn
        (kill-this-buffer)
        (tide-jump-back)))

   (require 'tide)
   (flycheck-add-mode 'typescript-tide 'web-mode)
   (flycheck-add-mode 'javascript-tide 'web-mode)

   (flycheck-add-mode 'javascript-eslint 'web-mode)
   ;; (flycheck-add-next-checker 'jsx-tide '(warning . javascript-eslint) 'append)
   ;; (flycheck-add-next-checker 'javascript-tide '(warning . javascript-eslint) 'append)
   (flycheck-add-next-checker 'tsx-tide '(warning . javascript-eslint) 'append)
   (flycheck-add-next-checker 'typescript-tide '(warning . javascript-eslint) 'append)
   )

  ;; enable prettier mode
  ;; https://github.com/prettier/prettier-emacs/issues/29
  (when (maybe-require-package 'prettier-js)
    (defun maybe-use-prettier ()
      "Enable prettier-js-mode if an rc file is located."
      (if (or
            (locate-dominating-file default-directory ".prettierrc.json")
            (locate-dominating-file default-directory ".prettierrc.js")
            (locate-dominating-file default-directory ".prettierrc"))
          (prettier-js-mode)))

    (add-hook 'web-mode-hook 'maybe-use-prettier)
    (add-hook 'rjsx-mode-hook 'maybe-use-prettier))
)

(provide 'init-typescript)
