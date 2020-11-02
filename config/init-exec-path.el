;;; init-exec-path.el --- Set up exec-path to help Emacs find programs  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'exec-path-from-shell)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setenv "PATH" (concat (getenv "PATH") (substitute-in-file-name ":$HOME/bin")))
(setq exec-path (append exec-path '(substitute-in-file-name "$HOME/bin")))
(setenv "PATH" (concat (getenv "PATH") (substitute-in-file-name ":$HOME/.local/bin")))
(setq exec-path (append exec-path '(substitute-in-file-name "$HOME/.local/bin")))
;; (exec-path-from-shell-copy-env "GOPATH")
(setenv "GOPATH" (substitute-in-file-name "$HOME/go"))
(setenv "PROXY_REPO"  (substitute-in-file-name "$HOME/null/.proxy-repo"))

(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/.cargo/bin")))
(setq exec-path (append exec-path (list (expand-file-name "~/.cargo/bin"))))

;; rust 
;; (setenv "PATH" (concat (getenv "PATH") ":~/.cargo/bin"))
;; (setq exec-path (append exec-path '("~/.cargo/bin")))


(provide 'init-exec-path)
;;; init-exec-path.el ends here
