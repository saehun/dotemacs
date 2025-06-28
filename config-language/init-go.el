;;; init-go.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Complete Go development environment using go-ts-mode with gopls LSP
;;;; Requirements:
;; - gopls installed: go install golang.org/x/tools/gopls@latest
;; - goimports installed: go install golang.org/x/tools/cmd/goimports@latest
;; - golangci-lint installed: go install github.com/golangci/golangci-lint/cmd/golangci-lint@latest

;;; Code:

;; Go module files
(add-to-list 'auto-mode-alist '("\\.mod$" . go-mod-ts-mode))

;; Main go-ts-mode configuration
(use-package go-ts-mode
  :ensure t
  :defer t
  :mode ("\\.go\\'" . go-ts-mode)
  :hook ((go-ts-mode . lsp-deferred)
         (go-ts-mode . lsp-go-install-save-hooks)
         (go-ts-mode . yas-minor-mode)
         (go-ts-mode . (lambda ()
                         (setq-local tab-width 4)
                         (setq-local indent-tabs-mode t))))
  :bind (:map go-ts-mode-map
              ("C-c C-t" . go-test-current-test)
              ("C-c C-f" . go-test-current-file)
              ("C-c C-p" . go-test-current-project)
              ("C-c C-b" . go-test-current-benchmark)
              ("C-c C-r" . lsp-rename)
              ("C-c C-d" . lsp-describe-thing-at-point)
              ("C-c C-o" . lsp-organize-imports)
              ("C-c C-a" . lsp-execute-code-action)
              ("M-." . lsp-find-definition)
              ("M-," . pop-tag-mark)
              ("M-?" . lsp-find-references))
  :config
  ;; Set indentation
  (setq go-ts-mode-indent-offset 4)
  
  ;; Format and organize imports before save
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t)))

;; Configure gopls via LSP
(with-eval-after-load 'lsp-mode
  ;; Performance tuning
  (setq lsp-gopls-server-path "gopls"
        lsp-gopls-use-placeholders t
        lsp-gopls-staticcheck t
        lsp-gopls-complete-unimported t
        lsp-gopls-matcher "CaseSensitive"
        lsp-gopls-symbol-matcher "CaseSensitive"
        lsp-gopls-symbol-scope "workspace"
        lsp-gopls-codelenses '((generate . t)
                               (test . t)
                               (tidy . t)
                               (upgrade_dependency . t)
                               (vendor . t)
                               (gc_details . t))
        lsp-gopls-analyses '((fieldalignment . t)
                             (nilness . t)
                             (shadow . t)
                             (unusedparams . t)
                             (unusedwrite . t)
                             (useany . t)
                             (unusedvariable . t)))
  
  ;; Enable inlay hints
  (setq lsp-inlay-hint-enable t)
  
  ;; Custom gopls settings
  (lsp-register-custom-settings
   '(("gopls.hints.assignVariableTypes" t t)
     ("gopls.hints.compositeLiteralFields" t t)
     ("gopls.hints.compositeLiteralTypes" t t)
     ("gopls.hints.constantValues" t t)
     ("gopls.hints.functionTypeParameters" t t)
     ("gopls.hints.parameterNames" t t)
     ("gopls.hints.rangeVariableTypes" t t)
     ("gopls.usePlaceholders" t t)
     ("gopls.vulncheck" "Imports" t))))

;; Go specific packages

;; Gotest - Enhanced test running
(use-package gotest
  :ensure t
  :after go-ts-mode
  :bind (:map go-ts-mode-map
              ("C-c t t" . go-test-current-test)
              ("C-c t f" . go-test-current-file)
              ("C-c t p" . go-test-current-project)
              ("C-c t b" . go-test-current-benchmark)
              ("C-c t c" . go-test-current-coverage)
              ("C-c t T" . go-test-current-test-cache))
  :config
  (setq go-test-verbose t))

;; Go-tag - Struct tag management
(use-package go-tag
  :ensure t
  :after go-ts-mode
  :bind (:map go-ts-mode-map
              ("C-c t a" . go-tag-add)
              ("C-c t r" . go-tag-remove)))

;; Go-gen-test - Generate test files
(use-package go-gen-test
  :ensure t
  :after go-ts-mode
  :bind (:map go-ts-mode-map
              ("C-c t g" . go-gen-test-dwim)))

;; Go-impl - Generate interface implementations
(use-package go-impl
  :ensure t
  :after go-ts-mode
  :bind (:map go-ts-mode-map
              ("C-c i i" . go-impl)))

;; Go-fill-struct - Fill struct literals
(use-package go-fill-struct
  :ensure t
  :after go-ts-mode
  :bind (:map go-ts-mode-map
              ("C-c i f" . go-fill-struct)))

;; Godoctor - Go refactoring
(use-package godoctor
  :ensure t
  :after go-ts-mode
  :bind (:map go-ts-mode-map
              ("C-c r r" . godoctor-rename)
              ("C-c r e" . godoctor-extract)
              ("C-c r t" . godoctor-toggle)
              ("C-c r d" . godoctor-godoc)))

;; Flycheck-golangci-lint - Enhanced linting
(use-package flycheck-golangci-lint
  :ensure t
  :hook (go-ts-mode . flycheck-golangci-lint-setup)
  :config
  (setq flycheck-golangci-lint-tests t
        flycheck-golangci-lint-fast t
        flycheck-golangci-lint-enable-all t))

;; Helper functions for Go development

(defun go-run-main ()
  "Run the main function in the current buffer."
  (interactive)
  (compile "go run ."))

(defun go-run-tests-regexp (regexp)
  "Run Go tests matching REGEXP."
  (interactive "sTest regexp: ")
  (compile (format "go test -v -run %s" regexp)))

(defun go-mod-tidy ()
  "Run go mod tidy."
  (interactive)
  (compile "go mod tidy"))

(defun go-mod-download ()
  "Run go mod download."
  (interactive)
  (compile "go mod download"))

;; Bind helper functions
(with-eval-after-load 'go-ts-mode
  (define-key go-ts-mode-map (kbd "C-c m r") 'go-run-main)
  (define-key go-ts-mode-map (kbd "C-c m t") 'go-mod-tidy)
  (define-key go-ts-mode-map (kbd "C-c m d") 'go-mod-download)
  (define-key go-ts-mode-map (kbd "C-c t R") 'go-run-tests-regexp))

;; Snippets directory for Go (if using yasnippet)
(with-eval-after-load 'yasnippet
  (add-to-list 'yas-snippet-dirs
               (expand-file-name "snippets/go-mode" user-emacs-directory) t))

(message "init-go.el loaded")
(provide 'init-go)
;;; init-go.el ends here
