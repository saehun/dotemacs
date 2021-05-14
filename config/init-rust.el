;;; init-rust.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; based on this article
;; https://robert.kra.hn/posts/2021-02-07_rust-with-emacs/

;; System dependencies
;; rust-analyzer
;; rust

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("C-c r" . lsp-find-references)
              ("C-c d" . lsp-find-definition)
              ("C-c C-r" . lsp-rename)
              ("C-c C-f" . lsp-execute-code-action)
              ("M-s-r" . lsp-workspace-restart))

  :config
  ;; uncomment for less flashiness
  (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (setq lsp-ui-doc-mode nil)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm
  (setq-local buffer-save-without-query t))

(provide 'init-rust)
;;; init-rust.el ends here
