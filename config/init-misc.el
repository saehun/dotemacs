;;----------------------------------------------------------------------------
;; recent file mode
;;----------------------------------------------------------------------------
(recentf-mode 1)
(setq recentf-max-menu-items 1000)
(global-set-key "\C-x\ \C-r" 'counsel-recentf)

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
(when (maybe-require-package 'editorconfig)
  (add-hook 'prog-mode-hook 'editorconfig-mode))

(when (maybe-require-package 'ibuffer-projectile)
  (add-hook 'ibuffer-hook
      (lambda ()
        (ibuffer-projectile-set-filter-groups)
        (unless (eq ibuffer-sorting-mode 'alphabetic)
          (ibuffer-do-sort-by-alphabetic)))))

(maybe-require-package 'yaml-mode)

(when (maybe-require-package 'drag-stuff)
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

(when (maybe-require-package 'dired-ranger)
  (define-key dired-mode-map (kbd "y") 'dired-ranger-copy)
  (define-key dired-mode-map (kbd "p") 'dired-ranger-paste))


(when (maybe-require-package 'golden-ratio)
  (golden-ratio-mode 1))

;;----------------------------------------------------------------------------
;; yaml lint
;;----------------------------------------------------------------------------
(when (maybe-require-package 'flycheck-yamllint)
  (add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))

;; enable color emoji
(set-fontset-font t 'symbol "Apple Color Emoji")
(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
(set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
(set-fontset-font t 'symbol "Symbola" nil 'append)

(provide 'init-misc)
