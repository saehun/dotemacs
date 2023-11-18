;;; init-misc.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; 
;;;; Requirements:
(require 'dired-ranger)
(require 'drag-stuff)
(require 'editorconfig)
(require 'ibuffer-projectile)
(require 'package)
(require 'recentf)
(require 'yaml-mode)

;;; Code:

;;----------------------------------------------------------------------------
;; recent file mode
;;----------------------------------------------------------------------------
(recentf-mode 1)
(setq recentf-max-menu-items 50
      recentf-max-saved-items 2000
      recentf-exclude '("/Users/saehun/Documents/Mail.+"
                        "/Users/saehun/Documents/Notes.+"
                        "/opt/homebrew/.+"
                        ))
(let (message-log-max)
  (recentf-mode 1))

;;----------------------------------------------------------------------------
;; Miscellaneous
;;----------------------------------------------------------------------------
(electric-pair-mode 1)
(defmacro custom/add-mode-pairs (hook pairs)
  `(add-hook ,hook
             (lambda ()
               (setq-local electric-pair-pairs (append electric-pair-pairs ,pairs))
               (setq-local electric-pair-text-pairs electric-pair-pairs))))

(custom/add-mode-pairs 'web-mode-hook '((?` . ?`)))
(custom/add-mode-pairs 'web-mode-hook '((?' . ?')))
;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)


;;----------------------------------------------------------------------------
;; editorconfig
;;----------------------------------------------------------------------------
(add-hook 'prog-mode-hook 'editorconfig-mode)

(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-projectile-set-filter-groups)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))


(drag-stuff-global-mode 1)
(drag-stuff-define-keys)

(define-key dired-mode-map (kbd "y") 'dired-ranger-copy)
(define-key dired-mode-map (kbd "p") 'dired-ranger-paste)
(define-key dired-mode-map (kbd "X") 'dired-ranger-move)



;;----------------------------------------------------------------------------
;; yaml lint
;;----------------------------------------------------------------------------
(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup)

;; enable color emoji
(set-fontset-font t 'symbol "Apple Color Emoji")
(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
(set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
(set-fontset-font t 'symbol "Symbola" nil 'append)


(message "init-misc.el")
(provide 'init-misc)
;;; init-misc.el ends here
