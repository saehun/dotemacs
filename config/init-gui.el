(when (maybe-require-package 'tabbar)
  (tabbar-mode 1)
  (set-face-attribute 'tabbar-unselected nil :foreground "gray50")
  (setq tabbar-separator (quote (0.5))))
(when (maybe-require-package 'beacon)
  (add-hook 'after-init-hook 'beacon-mode))

(setq-default cursor-type 'bar)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-frame-font "Monaco:pixelsize=14")
(setq-default line-spacing -1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

;;----------------------------------------------------------------------------
;; http://ivanmalison.github.io/dotfiles/
;;----------------------------------------------------------------------------
(use-package nlinum
  :disabled t
  :demand t
  :config
  (progn
    (add-hook 'prog-mode-hook (lambda () (nlinum-mode t)))
    (defun imalison-nlinum-mode-hook ()
      (when nlinum-mode
        (setq-local nlinum-format
                    (concat "%" (number-to-string
                                 ;; Guesstimate number of buffer lines.
                                 (ceiling (log (max 1 (/ (buffer-size) 80)) 10)))
                            "d"))))))
(global-linum-mode 1)
(global-hl-line-mode 1)


;;----------------------------------------------------------------------------
;; title bar
;;----------------------------------------------------------------------------
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;;----------------------------------------------------------------------------
;; No popup frame @ivanmalison.github.io
;;----------------------------------------------------------------------------
(setq ns-pop-up-frames nil)
(setq pop-up-frames nil)

;;----------------------------------------------------------------------------
;; No dialog box @ivanmalison.github.io
;;----------------------------------------------------------------------------
(setq use-dialog-box nil)


;;----------------------------------------------------------------------------
;; Time in modeline @ivanmalison.github.io
;;----------------------------------------------------------------------------
;; (setq display-time-default-load-average nil)
;; (setq display-time-interval 1)
;; (setq display-time-format "%a|%m-%d|%r")
;; (display-time-mode +1)

(provide 'init-gui)