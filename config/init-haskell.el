;;; init-haskell.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'haskell-mode)
  (maybe-require-package 'lsp-haskell)
  (with-eval-after-load "haskell-mode"
    ;; This changes the evil "O" and "o" keys for haskell-mode to make sure that
    ;; indentation is done correctly. See
    ;; https://github.com/haskell/haskell-mode/issues/1265#issuecomment-252492026.
    (defun haskell-evil-open-above ()
      (interactive)
      (evil-digit-argument-or-evil-beginning-of-line)
      (haskell-indentation-newline-and-indent)
      (evil-previous-line)
      (haskell-indentation-indent-line)
      (evil-append-line nil))
  
    (defun haskell-evil-open-below ()
      (interactive)
      (evil-append-line nil)
      (haskell-indentation-newline-and-indent))
  
    (evil-define-key 'normal haskell-mode-map
      "o" 'haskell-evil-open-below
      "O" 'haskell-evil-open-above)
  )

  ;; (when (maybe-require-package 'flycheck-haskell)
  ;;   (add-hook 'haskell-mode-hook #'flycheck-haskell-setup))
  (custom-set-variables
    '(haskell-stylish-on-save t))

  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp)) 


(provide 'init-haskell)
;;; init-haskell.el ends here
