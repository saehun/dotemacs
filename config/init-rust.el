;;; init-rust.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; based on this article
;; http://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-rust-editor-from-scratch/

;; System dependencies
;; racer
;; rust

(when (maybe-require-package 'rust-mode)

  (maybe-require-package 'flycheck-rust)

  ;; cargo-mode for all the cargo related operations
  ;; https://github.com/kwrooijen/cargo.el
  (when (maybe-require-package 'cargo)
    (add-hook 'rust-mode-hook 'cargo-minor-mode))

  ;; racer-mode for getting IDE like features for rust-mode
  ;; https://github.com/racer-rust/emacs-racer
  (when (maybe-require-package 'racer)
    (add-hook 'rust-mode-hook #'racer-mode)

  ;; enable company and eldoc minor modes in rust-mode (racer-mode)
    (add-hook 'racer-mode-hook 'company-mode)
    (add-hook 'racer-mode-hook 'eldoc-mode)
    (add-hook 'rust-mode-hook 'flycheck-mode))

  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)

  ;; https://github.com/racer-rust/emacs-racer/issues/85
  (setq racer-rust-src-path
    (concat (string-trim
              (shell-command-to-string "rustc --print sysroot"))
      "/lib/rustlib/src/rust/library"))
  (setq flycheck-rust-cargo-executable "~/.cargo/bin/cargo")
  (setq flycheck-rust-executable "~/.cargo/bin/rustc")
  (setq flycheck-rust-clippy-executable "~/.cargo/bin/cargo")

  ;; format rust buffers on save using rustfmt
  (add-hook 'before-save-hook
    (lambda ()
      (when (eq major-mode 'rust-mode)
        (rust-format-buffer)))))

(provide 'init-rust)
;;; init-rust.el ends here
