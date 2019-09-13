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
(require 'init-bindings)
(require 'custom-macro)



