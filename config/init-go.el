;;; init-go.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; based on this article
;; http://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch/

;; System dependencies
;; go get -u github.com/nsf/gocode # to install gocode
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
      (setq gofmt-command "goimports")
      (add-hook 'before-save-hook 'gofmt-before-save)
      (setq truncate-lines t)
      (setq indent-tabs-mode t)
      (setq tab-width 4)))

  (when (maybe-require-package 'go-eldoc)
    (add-hook 'go-mode-hook 'go-eldoc-setup))

  (when (maybe-require-package 'go-guru)
    (add-hook 'go-mode-hook 'go-guru-hl-identifier-mode))

  (when (maybe-require-package 'company-go))
    (add-hook 'go-mode-hook (lambda ()
       (set (make-local-variable 'company-backends) '(company-go))
                              (company-mode)))

  ;; key bindings
  (define-key go-mode-map (kbd "C-c d") 'godef-jump)
  (define-key go-mode-map (kbd "C-c r") 'go-guru-referrers))



(provide 'init-go)
;;; init-go.el ends here
