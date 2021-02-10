;; typescript init

(when (maybe-require-package 'web-mode)
  (setq web-mode-enable-auto-quoting nil)
  (when (maybe-require-package 'tide)
    (defun setup-tide-mode ()
      (interactive)
      (tide-setup)
      (flycheck-mode +1)
      (setq flycheck-check-syntax-automatically '(save mode-enabled))
      ;; (setq tide-tsserver-executable "node_modules/typescript/bin/tsserver")
      (eldoc-mode +1)
      (tide-hl-identifier-mode +1)
      ;; (setq tide-completion-detailed t)
      ;; company is an optional dependency. You have to
      ;; install it separately via package-install
      ;; `M-x package-install [ret] company`
      (company-mode +1))
    ;; aligns annotation to the right hand side

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

    ;; enable typescript-tslint checker
    ;; at .emacs.d/elpa/tide/tide.el


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
      (if (locate-dominating-file default-directory ".prettierrc.json")
          (prettier-js-mode)))

    (add-hook 'web-mode-hook 'maybe-use-prettier)
    (add-hook 'rjsx-mode-hook 'maybe-use-prettier))
)

(provide 'init-typescript)
