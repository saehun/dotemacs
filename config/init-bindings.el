;;; package --- init-bindings
;;; Commentary:
;;; Code:

(defun with-safe (command)
  "Check whether minibuffer opened, running COMMAND, close seamlessly."
  `(lambda (&rest args)
     (interactive)
     (if (minibuffer-window-active-p (active-minibuffer-window))
       (minibuffer-keyboard-quit)
       (apply #',command args))))

(global-set-key (kbd "C-e") (with-safe 'counsel-M-x))
(global-set-key (kbd "s-F") (with-safe 'counsel-rg))
(global-set-key (kbd "M-s-f") (with-safe 'counsel-rg-here))
(global-set-key (kbd "s-G") (with-safe 'counsel-google))
(global-set-key (kbd "C-b") (with-safe 'counsel-switch-buffer))
(global-set-key (kbd "C-n") (with-safe 'counsel-find-file))
(global-set-key (kbd "s-p") (with-safe 'find-file-in-project))
(global-set-key (kbd "s-f") 'swiper)
(global-set-key (kbd "C-s-m") 'counsel-evil-goto-global-marker)


;;----------------------------------------------------------------------------
;; Buffer and windows
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-s") 'save-buffer)            ;; save
(global-set-key (kbd "s-w") 'kill-this-buffer)       ;; close의 의미
(global-set-key (kbd "C-`") 'other-window)       ;; Switch window
(global-set-key (kbd "C-~") 'copy-buffers-in-windows) ;; copy-buffers-in-windows
(global-set-key (kbd "s-b") 'ibuffer)            ;; list buffer
(global-set-key (kbd "s-t")  'new-empty-buffer)
(global-set-key (kbd "C-c \`")  'window-swap-states)
(global-set-key (kbd "<C-S-tab>") 'previous-buffer)
(global-set-key (kbd "<C-tab>") 'quit-window)
(global-set-key (kbd "s-{") 'tabbar-backward-tab)
(global-set-key (kbd "s-}") 'tabbar-forward-tab)
(global-set-key (kbd "C-s-{") 'tabbar-backward-group)
(global-set-key (kbd "C-s-}") 'tabbar-forward-group)
(global-set-key (kbd "C-s-n") 'eyebrowse-switch-to-window-config)
(global-set-key (kbd "<C-s-left>") 'windmove-left)
(global-set-key (kbd "<C-s-right>") 'windmove-right)
(global-set-key (kbd "<C-s-up>") 'windmove-up)
(global-set-key (kbd "<C-s-down>") 'windmove-down)

;;----------------------------------------------------------------------------
;; Evil
;;----------------------------------------------------------------------------
(require 'evil)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-word-mode)
(define-key evil-normal-state-map (kbd "g s") 'magit-status)
(define-key evil-normal-state-map (kbd "g a") 'magit-stage-file)
(define-key evil-normal-state-map (kbd "g c") 'magit-commit-current-file)
(define-key evil-normal-state-map (kbd "g b") 'magit-blame-echo)
(define-key evil-normal-state-map (kbd "C-@") 'flycheck-next-error)
(define-key evil-normal-state-map (kbd "C-!") 'counsel-flycheck)
(define-key evil-inner-text-objects-map "b" 'evil-textobj-anyblock-inner-block)
(define-key evil-outer-text-objects-map "b" 'evil-textobj-anyblock-a-block)
(define-key evil-normal-state-map "\C-n" nil)
(define-key evil-normal-state-map "\C-t" nil)
(define-key evil-normal-state-map "\C-p" nil)
(define-key evil-normal-state-map "\C-e" nil)
(define-key evil-normal-state-map "\C-z" nil)
(define-key evil-motion-state-map "\C-w" nil)
(define-key evil-motion-state-map "\C-b" nil)
(define-key evil-motion-state-map "\C-e" nil)
(define-key evil-motion-state-map "\C-f" nil)
(define-key evil-motion-state-map "\C-o" nil)
(define-key evil-motion-state-map "\C-z" nil)
(define-key evil-insert-state-map "\C-p" 'ivy-yasnippet)

(global-set-key (kbd "<s-right>") 'forward-word)
(global-set-key (kbd "<s-left>") 'backward-word)

;;----------------------------------------------------------------------------
;; Edit
;;----------------------------------------------------------------------------
(global-set-key (kbd "<f17>") 'toggle-input-method)
(global-set-key (kbd "s-/") 'comment-line)

;;----------------------------------------------------------------------------
;; Misc
;;----------------------------------------------------------------------------
(global-set-key (kbd "s-r") 'shell-command)          ;; run command
(global-set-key (kbd "C-SPC") 'company-complete)
(global-set-key (kbd "C-x C-e") 'counsel-shell-command)
(global-set-key (kbd "s-1") 'open-todo)


;;----------------------------------------------------------------------------
;; Unset
;;----------------------------------------------------------------------------
(global-unset-key "\C-z")

;;----------------------------------------------------------------------------
;; Typescript
;;----------------------------------------------------------------------------
(require 'tide)
(define-key tide-mode-map (kbd "C-c r") 'tide-references)
(define-key tide-mode-map (kbd "C-c f") 'tide-refactor)
(define-key tide-mode-map (kbd "C-c d") 'tide-jump-to-definition)
(define-key tide-mode-map (kbd "C-c C-r") 'tide-rename-symbol)
(define-key tide-mode-map (kbd "C-c C-f") 'tide-fix)
(define-key tide-mode-map (kbd "C-c C-e") 'tide-project-errors)
(define-key tide-mode-map (kbd "C-c <delete>") 'tide-jump-back-and-kill)
(define-key tide-references-mode-map (kbd "<return>") 'tide-goto-reference)

;;----------------------------------------------------------------------------
;; Web-mode
;;----------------------------------------------------------------------------
(require 'web-mode)
(define-key web-mode-map (kbd "C-c C-a") 'web-mode-attribute-select)
(define-key web-mode-map (kbd "C-c C-c") 'web-mode-element-content-select)
(define-key web-mode-map (kbd "C-c C-t") 'web-mode-tag-select)
(define-key web-mode-map (kbd "C-c C-e") 'web-mode-element-select)
(define-key web-mode-map (kbd "C-c C-w") 'web-mode-tag-end)
;; (define-key web-mode-map (kbd "C-c C-r") 'web-mode-element-rename)
(define-key web-mode-map (kbd "C-c C-d") 'web-mode-attribute-kill)
(define-key web-mode-map (kbd "C-c f") 'fold-active-region)

(global-set-key (kbd "s-[") 'er/contract-region)        ;; expand region plugin
(global-set-key (kbd "s-]") 'er/expand-region)        ;; expand region plugin

;; (global-set-key (kbd "s-[") 'tabbar-move-current-tab-one-place-left)
;; (global-set-key (kbd "s-]") 'tabbar-move-current-tab-one-place-right)

(require 'magit)
(define-key magit-mode-map (kbd "C-d") 'evil-scroll-down)
(define-key magit-mode-map (kbd "C-u") 'evil-scroll-up)
(define-key magit-mode-map (kbd "<down>") 'magit-section-forward)
(define-key magit-mode-map (kbd "<up>") 'magit-section-backward)
(define-key magit-mode-map (kbd "<C-tab>") 'quit-window)

;; (global-set-key (kbd "M-r") 'query-replace)          ;; replace
;; (global-set-key (kbd "C-x o") 'open-file-at-cursor)        ;; open file under the cursor
;; (global-set-key (kbd "C-c l") 'goto-last-change)
;; (global-set-key (kbd "C-c p") 'yas/expand)
;; (global-set-key (kbd "s-2") 'jump-to-cursor)
(global-set-key (kbd "<s-backspace>") 'go-up-general)

;;----------------------------------------------------------------------------
;; rust
;;----------------------------------------------------------------------------
(require 'rustic)
(define-key rustic-mode-map (kbd "C-c d") 'racer-find-definition)

(provide 'init-bindings)
