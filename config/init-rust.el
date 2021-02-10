;;; init-rust.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; based on this article
;; http://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-rust-editor-from-scratch/

;; System dependencies
;; racer
;; rust

(use-package rustic)
(push 'rustic-clippy flycheck-checkers)
;;   :init
;;   (setq rustic-lsp-client 'eglot))

;; (require 'eglot)
;; (add-hook 'eglot-managed-mode-hook (lambda () (flymake-mode -1)))

(provide 'init-rust)
;;; init-rust.el ends here
