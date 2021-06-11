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
      (setq tide-completion-show-source t)
      (setq tide-completion-ignore-case t)
      ;; (setq tide-tsserver-executable "node_modules/typescript/bin/tsserver")

      (eldoc-mode +1)
      ;; (tide-hl-identifier-mode +1)
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
            (cl-search "toss/frontend-devops" project-root)
            (cl-search "toss/toss-frontend" project-root))
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

    (add-hook 'js2-mode-hook #'setup-tide-mode)


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


  ;; ---------------------------------- Yarn2 support 
  (require 'arc-mode)

  (defun tide--get-arc-path-pair (full-path)
    ;; \\|\\\\ in case of some substandard OSes ;)
    ;; Is there some safer/more-generic way to split/join paths in elisp?
    ;; Possibly, OS-neutral and with URL/protocol prefixes?
    (let ((path-components (split-string full-path "/\\|\\\\"))
           (arc-path "")
           (file-path-in-arc "")
           arc-found)
      ;; Distinguishing absolute and relative paths - i.e. trailing "/".
      (unless (string-empty-p (car path-components))
        (setq arc-path (car path-components)))
      (setq path-components (cdr path-components))
      (seq-do
        (lambda (component)
          (if arc-found
            (setq file-path-in-arc (concat file-path-in-arc "/" component))
            (setq arc-path (concat arc-path "/" component))
            (when (and (file-regular-p arc-path)
                    (with-temp-buffer
                      ;; 300000 is a magic number - it should
                      ;; be more than enough to recognise any achieve
                      ;; type header.
                      (insert-file-contents arc-path nil 0 300000)
                      (ignore-errors (archive-find-type))))
              (setq arc-found t))))
        path-components)
      (and arc-found
        (not (string-empty-p arc-path))
        (not (string-empty-p file-path-in-arc))
        (cons arc-path (substring file-path-in-arc 1)))))

  (defun tide-get-file-buffer:override (file &optional new-file)
    "Returns a buffer associated with a file. This will return the
current buffer if it matches `file'. This way we can support
temporary and indirect buffers."
    (let ((file-virtual-resolved
            (replace-regexp-in-string "\\.yarn/__virtual.*/[0-9]+/" "" file))
           arc-path-pair)
      (cond
        ((equal file (tide-buffer-file-name)) (current-buffer))
        ((setq arc-path-pair
           (tide--get-arc-path-pair
             (replace-regexp-in-string "__$virtual.*cache/" "cache/" file)))
          (let ((arc-path (car arc-path-pair))
                 (file-path-in-arc (cdr arc-path-pair))
                 arc-buf)
            (setq arc-buf (find-file-noselect arc-path))
            (with-current-buffer arc-buf
              (goto-char (point-min))
              (re-search-forward (concat " " file-path-in-arc "$"))
              (archive-extract))))
        ((file-exists-p file) (find-file-noselect file))
        ((file-exists-p file-virtual-resolved)
          (find-file-noselect file-virtual-resolved))
        (new-file (let ((buffer (create-file-buffer file)))
                    (with-current-buffer buffer
                      (set-visited-file-name file)
                      (basic-save-buffer)
                      (display-buffer buffer t))
                    buffer))
        (t (error "Invalid file %S" file)))))

  (advice-add 'tide-get-file-buffer :override
    #'tide-get-file-buffer:override)
  ;; --------------------------------------------------------------- Yarn2 support

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
    (add-hook 'js2-mode-hook 'maybe-use-prettier))
)

(provide 'init-typescript)
