;;; init-gui.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;----------------------------------------------------------------------------
;; Beacon - disabled - performance issue
;;----------------------------------------------------------------------------
;;  (when (maybe-require-package 'beacon)
;;    (add-hook 'after-init-hook 'beacon-mode))

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
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . fullheight))

;;----------------------------------------------------------------------------
;; linum  disable --- for performance issue
;;----------------------------------------------------------------------------
;; (global-linum-mode 1)
;; (global-hl-line-mode 1)
;; (setq linum-format "%d ")

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
  ;; (require 'doom-subliminal)
  (load-theme 'doom-dark+ t)
  )
(maybe-require-package 'gruber-darker-theme)


;;----------------------------------------------------------------------------
;; Tabbar  
;;----------------------------------------------------------------------------
(when (maybe-require-package 'tabbar)

  (tabbar-mode 1)
  (let ((custom-tabbar-bg (face-attribute 'default :background)))
    

    (customize-set-variable 'tabbar-separator '(0.7))
    (customize-set-variable 'tabbar-background-color custom-tabbar-bg)
    (customize-set-variable 'tabbar-use-images nil)

    ;; Colors
    (set-face-attribute 'tabbar-default nil
      :background custom-tabbar-bg 
      :foreground "gray60" 
      :box nil)
    (set-face-attribute 'tabbar-unselected nil
      :background custom-tabbar-bg 
      :foreground "gray60"  ;; hmmm
      ;; :box '(:line-width 10 :color custom-tabbar-bg))
      :box nil)
    (set-face-attribute 'tabbar-selected nil
      :background custom-tabbar-bg 
      :foreground "GoldenRod2" 
      :box nil)
    (set-face-attribute 'tabbar-modified nil
      :foreground "#f56476" :box nil
      :inherit 'tabbar-unselected)
    (set-face-attribute 'tabbar-selected-modified nil
      :inherit 'tabbar-selected 
      :foreground "GoldenRod2" 
      :box nil)
    (set-face-attribute 'tabbar-button nil :box nil)


;; (setq tabbar-separator (quote (0.5)))
    ))


(provide 'init-gui)
