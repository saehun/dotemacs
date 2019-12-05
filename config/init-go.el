;;; init-go.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; based on this article
;; http://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch/

;; System dependencies
;; go get golang.org/x/tools/cmd/goimports #
;; go get -u github.com/kisielk/errcheck
;; godef

(when (maybe-require-package 'go-mode)
  
  ;; setup gopath
  (push "/Users/minidonut/go/bin" exec-path)
  (setq exec-path (append exec-path '("/usr/local/go/bin")))
  (setenv "PATH" (concat "/usr/local/go/bin" ":" (getenv "PATH")))
  (setenv "PATH" (concat "/Users/minidonut/go/bin" ":" (getenv "PATH")))

  (add-hook 'go-mode-hook
    (lambda ()
      (setq truncate-lines t)
      (setq indent-tabs-mode t)
      (setq tab-width 4)
      (setq lsp-prefer-flymake nil)
      (setq lsp-eldoc-render-all t)))

  (when (maybe-require-package 'go-guru)
    (add-hook 'go-mode-hook 'go-guru-hl-identifier-mode))

  (use-package lsp-mode
    :ensure t
    :commands (lsp lsp-deferred)
    :hook (go-mode . lsp-deferred))

  ;; Set up before-save hooks to format buffer and add/delete imports.
  ;; Make sure you don't have other gofmt/goimports hooks enabled.
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

  ;; company-lsp integrates company mode completion with lsp-mode.
  ;; completion-at-point also works out of the box but doesn't support snippets.
  (use-package company-lsp
    :ensure t
    :commands company-lsp)

  (require 'go-mode)

  (let ((govet (flycheck-checker-get 'go-vet 'command)))
    (when (equal (cadr govet) "tool")
      (setf (cdr govet) (cddr govet))))

  (define-key go-mode-map (kbd "C-c d") 'godef-jump)
  (define-key go-mode-map (kbd "C-c r") 'go-guru-referrers))

(provide 'init-go)
;;; init-go.el ends here
