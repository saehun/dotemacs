;;; init-exec-path.el --- Set up exec-path to help Emacs find programs  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'exec-path-from-shell)

;; Environment variable
;; golang
(setenv "GOPATH"     (substitute-in-file-name "$HOME/go"))
;; ghq
(setenv "GHQ_ROOT"   (substitute-in-file-name "$HOME/repo"))
;; proxy-repo
(setenv "PROXY_REPO" (substitute-in-file-name "$HOME/null/.proxy-repo"))
;; default profile
(setenv "PROFILE" "local")
;; default NODE_ENV
(setenv "NODE_ENV" "development")
;; company zscalar setup
(if (string-equal user-login-name "saehun") (setenv "NODE_EXTRA_CA_CERTS" "/Users/Shared/zscaler.pem"))

;; default SERVICE_SECRET for some usage
;; you can delete anytime
(setenv "SERVICE_SECRET" "7oNck46AUmtd6AEV")

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

;; pnpm7 binary
(setenv "PATH" (concat (getenv "PATH") (substitute-in-file-name ":$HOME/Library/pnpm")))
(setq exec-path (append exec-path (list (substitute-in-file-name "$HOME/Library/pnpm"))))

;; my-custom binary
(setenv "PATH" (concat (getenv "PATH") (substitute-in-file-name ":$HOME/bin")))
(setq exec-path (append exec-path (list (substitute-in-file-name "$HOME/bin"))))

;; rust
(setenv "PATH" (concat (getenv "PATH") (substitute-in-file-name ":$HOME/.cargo/bin")))
(setq exec-path (append exec-path (list (substitute-in-file-name "$HOME/.cargo/bin"))))

;; Latex, see https://www.fromkk.com/posts/preview-latex-in-org-mode-with-emacs-in-macos/
(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))
(setq exec-path (append exec-path (list "/Library/TeX/texbin")))

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


