;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(require 'cl)
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
(require 'init-ffip)
;; (require 'init-hydra)
(require 'init-magit)


;;----------------------------------------------------------------------------
;; Programming
;;----------------------------------------------------------------------------
(require 'init-terraform)
(require 'init-typescript)
(require 'init-javascript)
(require 'init-markdown)
(require 'init-go)
(require 'init-haskell)
(require 'init-restclient)
(require 'init-rust)
;; (require 'init-cl)


;;----------------------------------------------------------------------------
;; Keyboard binding
;;----------------------------------------------------------------------------
(require 'init-yasnippet)
(require 'init-misc)
(require 'init-custom-fns)
(require 'init-bindings)

(require 'init-clojure)

(require 'node-string)
(require 'node-binding)
