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

;; font
;; (set-frame-font "Iosevka Term:pixelsize=13:weight=light:slant=normal:width=normal:spacing=100:scalable=true")
;; (set-frame-font "SF Mono:pixelsize=14:weight=light:slant=normal:width=normal:spacing=100:scalable=true")
;; (set-frame-font "")
(add-to-list 'default-frame-alist
             '(font . "Iosevka Term:pixelsize=14:weight=normal:slant=normal:width=normal:spacing=100:scalable=true"))
(add-to-list 'custom-theme-load-path "~/repo/github.com/minidonut/dotemacs/themes")


    ;; -*--light-normal-normal-*-*-*-*-*-m-0-iso10646-1

(setq-default line-spacing 0.12)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default truncate-lines t)
(setq indent-line-function 'insert-tab)
(show-paren-mode 1)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . fullheight))

;;----------------------------------------------------------------------------
;; linum  
;;----------------------------------------------------------------------------
(global-display-line-numbers-mode)
;; (global-hl-line-mode 1)
;; (setq linum-format "%d ")

;;----------------------------------------------------------------------------
;; title bar
;;----------------------------------------------------------------------------
(require 'projectile)
(setq frame-title-format '((:eval (projectile-project-name)) ": %b"))
(set-frame-parameter nil 'ns-appearance 'dark)
(set-frame-parameter nil 'ns-transparent-titlebar (string= (invocation-name) "emacs"))

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
;; Modeline
;;----------------------------------------------------------------------------
(when (maybe-require-package 'doom-modeline)
 (doom-modeline-mode 1)

  ;; https://github.com/seagle0128/doom-modeline/issues/164
  ;; (advice-add #'select-window :after #'doom-modeline-set-selected-window)
  ;; https://github.com/seagle0128/doom-modeline/issues/306
  ;; (custom-set-faces `(doom-modeline-icon-inactive ((t (:inherit mode-line-inactive)))))
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)
  (setq inhibit-compacting-font-caches t)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-height 20))


;;----------------------------------------------------------------------------
;; Theme
;;----------------------------------------------------------------------------
(when (maybe-require-package 'doom-themes)
  ;; (doom-themes-visual-bell-config) Disable visual bell
  (doom-themes-org-config)
  (setq doom-themes-enable-bold t
    doom-themes-enable-italic t)
  
  (load-theme
    (if (string= (invocation-name) "emacs")
      'doom-dark+
      ;;'doom-subliminal)
      'doom-dark+)
    t))
;; (maybe-require-package 'gruber-darker-theme)

(if (string= (invocation-name) "emacs") (server-start))



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

    ;; Here’s a example of tab group function that define all tabs to be one of two possible groups: “emacs” and “user”.
    (defun my-tabbar-buffer-groups () ;; customize to show all normal files in one group
      "Returns the name of the tab group names the current buffer belongs to.
    There are two groups: Emacs buffers (those whose name starts with '*', plus
    dired buffers), and the rest.  This works at least with Emacs v24.2 using
    tabbar.el v1.7."
      (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
              ((and
                 (>= (length (buffer-name)) 5)
                 (string-equal "magit" (substring (buffer-name) 0 5))) "magit")
              ((eq major-mode 'dired-mode) "dired")
                  (t "user"))))
    (setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

    ;; custom function
    (defun tabbar-move-current-tab-one-place-left ()
      "Move current tab one place left, unless it's already the leftmost."
      (interactive)
      (let* ((bufset (tabbar-current-tabset t))
             (old-bufs (tabbar-tabs bufset))
             (first-buf (car old-bufs))
             (new-bufs (list)))
        (if (string= (buffer-name) (format "%s" (car first-buf)))
            old-bufs ; the current tab is the leftmost
          (setq not-yet-this-buf first-buf)
          (setq old-bufs (cdr old-bufs))
          (while (and
                  old-bufs
                  (not (string= (buffer-name) (format "%s" (car (car old-bufs))))))
            (push not-yet-this-buf new-bufs)
            (setq not-yet-this-buf (car old-bufs))
            (setq old-bufs (cdr old-bufs)))
          (if old-bufs ; if this is false, then the current tab's buffer name is mysteriously missing
              (progn
                (push (car old-bufs) new-bufs) ; this is the tab that was to be moved
                (push not-yet-this-buf new-bufs)
                (setq new-bufs (reverse new-bufs))
                (setq new-bufs (append new-bufs (cdr old-bufs))))
            (error "Error: current buffer's name was not found in Tabbar's buffer list."))
          (set bufset new-bufs)
          (tabbar-set-template bufset nil)
          (tabbar-display-update))))

    (defun tabbar-move-current-tab-one-place-right ()
      "Move current tab one place right, unless it's already the rightmost."
      (interactive)
      (let* ((bufset (tabbar-current-tabset t))
             (old-bufs (tabbar-tabs bufset))
             (first-buf (car old-bufs))
             (new-bufs (list)))
        (while (and
                old-bufs
                (not (string= (buffer-name) (format "%s" (car (car old-bufs))))))
          (push (car old-bufs) new-bufs)
          (setq old-bufs (cdr old-bufs)))
        (if old-bufs ; if this is false, then the current tab's buffer name is mysteriously missing
            (progn
              (setq the-buffer (car old-bufs))
              (setq old-bufs (cdr old-bufs))
              (if old-bufs ; if this is false, then the current tab is the rightmost
                  (push (car old-bufs) new-bufs))
              (push the-buffer new-bufs)) ; this is the tab that was to be moved
          (error "Error: current buffer's name was not found in Tabbar's buffer list."))
        (setq new-bufs (reverse new-bufs))
        (setq new-bufs (append new-bufs (cdr old-bufs)))
        (set bufset new-bufs)
        (tabbar-set-template bufset nil)
        (tabbar-display-update)))

;; (setq tabbar-separator (quote (0.5)))
    ))


;;(when (maybe-require-package 'dimmer)
;;  (dimmer-mode t)
;;  (set-variable 'dimmer-fraction 0.3))

;;----------------------------------------------------------------------------
;; Awesome-tab
;;----------------------------------------------------------------------------
;;(use-package awesome-tab
;;  :load-path "../lisp/awesome-tab.el"
;;  :config
;;  (awesome-tab-mode t))

(provide 'init-gui)

