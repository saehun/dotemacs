;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "prisma" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

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
(require 'init-treemacs)
(require 'init-ffip)
(require 'init-hydra)
(require 'init-copilot)
(require 'init-magit)


;;----------------------------------------------------------------------------
;; Programming
;;----------------------------------------------------------------------------
(require 'init-terraform)
(require 'init-typescript)
(require 'init-typescript-extended)
(require 'init-tide-yarn2)
(require 'init-javascript)
(require 'init-markdown)
(require 'init-go)
(require 'init-haskell)
(require 'init-restclient)
(require 'init-rust)
(require 'init-clojure)

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

(require 'init-general-command)
(require 'init-machine)

;;; init.el ends here

