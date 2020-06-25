;;; init-clojure.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'clojure-mode)
  
  (maybe-require-package 'clojure-mode-extra-font-locking)
  (when (maybe-require-package 'cider)
    (add-hook 'cider-mode-hook 'eldoc-mode)
    (add-hook 'clojure-mode-hook #'cider-mode)

    ;; go right to the REPL buffer when it's finished connecting
    (setq cider-repl-pop-to-buffer-on-connect t)

    ;; When there's a cider error, show its buffer and switch to it
    (setq cider-show-error-buffer t)
    (setq cider-auto-select-error-buffer t)

    ;; Where to store the cider history.
    (setq cider-repl-history-file "~/null/cider-history")
    (add-hook 'cider-repl-mode-hook #'company-mode)
    (add-hook 'cider-mode-hook #'company-mode)
    (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
    (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)

    ;; Wrap when navigating history.
    (setq cider-repl-wrap-history t))


  (add-hook 'clojure-mode-hook
            (lambda ()
              (setq inferior-lisp-program "lein repl")
              (font-lock-add-keywords
              nil
              '(("(\\(facts?\\)"
                  (1 font-lock-keyword-face))
                ("(\\(background?\\)"
                  (1 font-lock-keyword-face))))
              (define-clojure-indent (fact 1))
              (define-clojure-indent (facts 1))
              (rainbow-delimiters-mode)))

  (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode)))



(provide 'init-clojure)
;;; init-clojure.el ends here
