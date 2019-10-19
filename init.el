;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(require 'init-system)
(require 'init-utils)
(require 'init-site-lisp)
(require 'init-elpa)
(require 'init-exec-path)

;;----------------------------------------------------------------------------
;; GUI config
;;----------------------------------------------------------------------------
(require 'init-gui)

;;----------------------------------------------------------------------------
;; Control
;;----------------------------------------------------------------------------
(require 'init-ivy)
(require 'init-evil)
(require 'custom-macro)
(require 'init-company)
(require 'init-flycheck)
(require 'init-ffip)
(require 'init-hydra)
(require 'init-magit)


;;----------------------------------------------------------------------------
;; Programming
;;----------------------------------------------------------------------------
(require 'init-terraform)
(require 'init-typescript)
(require 'init-markdown)


;;----------------------------------------------------------------------------
;; Keyboard binding
;;----------------------------------------------------------------------------
(require 'init-yasnippet)
(require 'init-misc)
(require 'init-bindings)
(require 'init-go)
