;;; init-exec-path.el --- Set up exec-path to help Emacs find programs  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'exec-path-from-shell)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setenv "PATH" (concat (getenv "PATH") ":/Users/root1/bin"))
(setq exec-path (append exec-path '("/Users/root1/bin")))
(setenv "PATH" (concat (getenv "PATH") ":/Users/root1/.local/bin"))
(setq exec-path (append exec-path '("/Users/root1/.local/bin")))
;; (exec-path-from-shell-copy-env "GOPATH")
(setenv "GOPATH" "/Users/root1/go")

(provide 'init-exec-path)
;;; init-exec-path.el ends here
