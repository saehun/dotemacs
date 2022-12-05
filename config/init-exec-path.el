;;; init-exec-path.el --- Set up exec-path to help Emacs find programs  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'exec-path-from-shell)

;; Environment variable
;; golang
(setenv "GOPATH"     (substitute-in-file-name "$HOME/go"))
;; ghq
(setenv "GHQ_ROOT"   (substitute-in-file-name "$HOME/repo"))
;; proxy-repo
(setenv "PROXY_REPO" (substitute-in-file-name "$HOME/null/.proxy-repo"))

;; Path variable
;; 3rd party binary
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; homebrew
(setenv "PATH" (concat (getenv "PATH") ":/opt/homebrew/bin"))
(setq exec-path (append exec-path '("/opt/homebrew/bin")))

;; 3rd party binary
(setenv "PATH" (concat (getenv "PATH") (substitute-in-file-name ":$HOME/.local/bin")))
(setq exec-path (append exec-path (list (substitute-in-file-name "$HOME/.local/bin"))))

;; yarn binary
(setenv "PATH" (concat (getenv "PATH") (substitute-in-file-name ":$HOME/.yarn/bin")))
(setq exec-path (append exec-path (list (substitute-in-file-name "$HOME/.yarn/bin"))))

;; my-custom binary
(setenv "PATH" (concat (getenv "PATH") (substitute-in-file-name ":$HOME/bin")))
(setq exec-path (append exec-path (list (substitute-in-file-name "$HOME/bin"))))

;; rust
(setenv "PATH" (concat (getenv "PATH") (substitute-in-file-name ":$HOME/.cargo/bin")))
(setq exec-path (append exec-path (list (substitute-in-file-name "$HOME/.cargo/bin"))))


;; haskell 
(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/.ghcup/bin")))
(setq exec-path (append exec-path (list (expand-file-name "~/.ghcup/bin"))))

;; haskell
;;(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/.cabal/bin")))
;;(setq exec-path (append exec-path (list (expand-file-name "~/.cabal/bin"))))

;; CMake
;;(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "/Applications/CMake.app/Contents/bin")))
;;(setq exec-path (append exec-path (list (expand-file-name "/Applications/CMake.app/Contents/bin"))))



(provide 'init-exec-path)
;;; init-exec-path.el ends here


