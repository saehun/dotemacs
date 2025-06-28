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

  ;;   ;; Enable/Disable inlay hints
  (setq lsp-inlay-hint-enable nil)

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

;; ;; Go specific packages

;; ;; Gotest - Enhanced test running
(require 'gotest)
(setq go-test-verbose t)

;; ;; Go-tag - Struct tag management
(require 'go-tag)

;; ;; Go-gen-test - Generate test files
(require 'go-gen-test)

;; ;; Go-impl - Generate interface implementations
(require 'go-impl)

;; ;; Go-fill-struct - Fill struct literals
(require 'go-fill-struct)

;; ;; Godoctor - Go refactoring
(require 'godoctor)

;; ;; Flycheck-golangci-lint - Enhanced linting
(require 'flycheck-golangci-lint)
(setq flycheck-golangci-lint-tests t
      flycheck-golangci-lint-fast t
      flycheck-golangci-lint-enable-all t)




;; Snippets directory for Go (if using yasnippet)
(require 'yasnippet)
(with-eval-after-load 'yasnippet
  (add-to-list 'yas-snippet-dirs
               (expand-file-name "snippets/go-mode" user-emacs-directory) t))

;; Basic key bindings for Go development (similar to tide)
(with-eval-after-load 'go-ts-mode
  (define-key go-ts-mode-map (kbd "C-c r") 'lsp-find-references)           ; tide-references
  (define-key go-ts-mode-map (kbd "C-r") 'lsp-execute-code-action)         ; my-tide-refactor
  (define-key go-ts-mode-map (kbd "C-c d") 'lsp-find-definition)          ; tide-jump-to-definition
  (define-key go-ts-mode-map (kbd "C-c C-f") 'lsp-execute-code-action)    ; my-tide-fix
  (define-key go-ts-mode-map (kbd "C-c C-r") 'lsp-rename)                 ; tide-rename-symbol
  (define-key go-ts-mode-map (kbd "C-c i") 'lsp-find-implementation)      ; tide-jump-to-implementation
  (define-key go-ts-mode-map (kbd "C-c s") 'xref-go-back)                 ; xref-go-back
  (define-key go-ts-mode-map (kbd "C-c h") 'lsp-describe-thing-at-point)) ; documentation

(message "init-go.el loaded")
(provide 'init-go)
;;; init-go.el ends here
