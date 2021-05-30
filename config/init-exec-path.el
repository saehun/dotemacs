;;; init-exec-path.el --- Set up exec-path to help Emacs find programs  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'exec-path-from-shell)

;; 3rd party binary
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; 3rd party binary
(setenv "PATH" (concat (getenv "PATH") (substitute-in-file-name ":$HOME/.local/bin")))
(setq exec-path (append exec-path '(substitute-in-file-name "$HOME/.local/bin")))

;; my-custom binary
(setenv "PATH" (concat (getenv "PATH") (substitute-in-file-name ":$HOME/bin")))
(setq exec-path (append exec-path '(substitute-in-file-name "$HOME/bin")))

;; golang
(setenv "GOPATH"     (substitute-in-file-name "$HOME/go"))
(setenv "GHQ_ROOT"   (substitute-in-file-name "$HOME/wd"))

;; rust
(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/.cargo/bin")))
(setq exec-path (append exec-path (list (expand-file-name "~/.cargo/bin"))))

;; haskell 
(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/.ghcup/bin")))
(setq exec-path (append exec-path (list (expand-file-name "~/.ghcup/bin"))))

;; haskell
(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/.cabal/bin")))
(setq exec-path (append exec-path (list (expand-file-name "~/.cabal/bin"))))

(setenv "PROXY_REPO" (substitute-in-file-name "$HOME/null/.proxy-repo"))


(provide 'init-exec-path)
;;; init-exec-path.el ends here


