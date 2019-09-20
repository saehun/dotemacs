;;; init-gui.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;----------------------------------------------------------------------------
;; Tabbar - disabled
;;----------------------------------------------------------------------------
(when (maybe-require-package 'tabbar)
  (tabbar-mode 1)
  (set-face-attribute 'tabbar-unselected nil :foreground "gray70")
  (setq tabbar-separator (quote (0.5))))


(when (maybe-require-package 'beacon)
  (add-hook 'after-init-hook 'beacon-mode))

(setq-default cursor-type 'bar)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-frame-font "Monaco:pixelsize=14")
(setq-default line-spacing 0.12)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)
(show-paren-mode 1)

;;----------------------------------------------------------------------------
;; linum
;;----------------------------------------------------------------------------
(global-linum-mode 1)
(global-hl-line-mode 1)
(setq linum-format "%d ")

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
(setq display-time-default-load-average nil)
(setq display-time-interval 1)
(setq display-time-format "%a|%m-%d|%r")
(display-time-mode +1)

;;----------------------------------------------------------------------------
;; Theme
;;----------------------------------------------------------------------------
(when (maybe-require-package 'doom-modeline)
  (doom-modeline-init))
(when (maybe-require-package 'doom-themes)
  ;; (doom-themes-visual-bell-config) Disable visual bell
  (doom-themes-org-config)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (require 'doom-subliminal))

(provide 'init-gui)
