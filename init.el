;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Code:

(dolist (dir '("config" "prisma" "lisp"))
  (add-to-list 'load-path (expand-file-name dir user-emacs-directory)))

;;----------------------------------------------------------------------------
;; Bootstrap package manager 'straight.el'
;;----------------------------------------------------------------------------
(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
     (bootstrap-version 6))
 (unless (file-exists-p bootstrap-file)
   (with-current-buffer
       (url-retrieve-synchronously
        "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
     (goto-char (point-max))
     (eval-print-last-sexp)))
 (load bootstrap-file nil 'nomessage))


(defvar package-list '(
  company
  consult             ; Consulting completing-read
  corfu               ; Completion Overlay Region FUnction
  dired-ranger
  doom-modeline
  doom-themes
  drag-stuff
  editorconfig
  eldoc-box           ; childframe doc for eglot and anything that uses eldoc
  evil                ; The extensible vi layer for Emacs.
  evil-surround
  evil-textobj-anyblock
  exec-path-from-shell; Get environment variables such as $PATH from the shell
  expand-region
  f                   ; Modern API for working with files and directories
  find-file-in-project
  flycheck
  flycheck-color-mode-line
  flycheck-yamllint
  flyspell-correct-popup ; Correcting words with flyspell via popup interface
  flyspell-popup      ; Correcting words with Flyspell in popup menus
  general             ; More convenient key definitions in emacs
  guess-language      ; Robust automatic language detection
  helpful             ; A better help buffer
  htmlize             ; Convert buffer text and decorations to HTML
  hydra
  ibuffer-projectile
  imenu-list          ; Show imenu entries in a separate buffer
  json-mode           ; Major mode for Json
  magit               ; A Git porcelain inside Emacs.
  marginalia          ; Enrich existing commands with completion annotations
  markdown-mode       ; Major mode for Markdown-formatted text
  markdown-toc
  mini-frame          ; Show minibuffer in child frame on read-from-minibuffer
  orderless           ; Completion style for matching regexps in any order
  projectile          ; Projectile
  request
  restclient
  savehist            ; Persist history over Emacs restarts.
  smex
  string-inflection   ; underscore -> UPCASE -> CamelCase conversion of names
  tide
  treemacs
  treemacs-projectile
  treesit-auto        ; Automatic installation, usage, and fallback for tree-sitter
  use-package         ; A configuration macro for simplifying your .emacs
  vertico             ; VERTical Interactive COmpletion
  wgrep
  which-key           ; Display available keybindings in popup
  yaml-mode           ; YAML mode
  yasnippet
  zoxide
     ))

;; Install packages that are not yet installed
(dolist (package package-list)
  (straight-use-package package))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(require 'init-system)
(require 'init-utils)
(require 'init-site-lisp)
(require 'init-elpa)
(require 'init-dired)
(require 'init-exec-path)

;;----------------------------------------------------------------------------
;; GUI config
;;----------------------------------------------------------------------------
(require 'init-gui)
(require 'init-eldoc)

;;----------------------------------------------------------------------------
;; Control
;;----------------------------------------------------------------------------
(require 'init-ivy)
(require 'init-evil)
(require 'custom-macro)
(require 'init-company)
(require 'init-flycheck)
(require 'init-treemacs)
(require 'init-ffip)
(require 'init-hydra)
(require 'init-magit)


;;----------------------------------------------------------------------------
;; Programming
;;----------------------------------------------------------------------------
(require 'init-typescript)
(require 'init-typescript-extended)
(require 'init-tide-yarn2)
(require 'init-javascript)
(require 'init-markdown)
(require 'init-go)
(require 'init-haskell)
(require 'init-restclient)
(require 'init-rust)

;;----------------------------------------------------------------------------
;; Minor mode
;;----------------------------------------------------------------------------
(require 'markdown-daily-mode)

;;----------------------------------------------------------------------------
;; Keyboard binding
;;----------------------------------------------------------------------------
(require 'init-yasnippet)
(require 'init-misc)
(require 'init-custom-fns)
(require 'init-bindings)
(require 'node-string)
(require 'node-binding)

;;----------------------------------------------------------------------------
;; hydra
;;----------------------------------------------------------------------------
(require 'init-hydra)
(require 'init-hydra-typescript)

(require 'init-general-command)
(require 'init-machine)

;;; init.el ends here

