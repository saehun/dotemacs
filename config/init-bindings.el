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
(global-set-key (kbd "s-f") 'swiper)
(global-set-key (kbd "s-F") (with-safe 'counsel-rg))
(global-set-key (kbd "M-s-f") (with-safe 'counsel-rg-package))
(global-set-key (kbd "M-s-F") (with-safe 'counsel-rg-here))
(global-set-key (kbd "s-G") (with-safe 'counsel-google))
(global-set-key (kbd "C-b") (with-safe 'counsel-switch-buffer))
(global-set-key (kbd "C-n") (with-safe 'counsel-find-file))
(global-set-key (kbd "s-p") (with-safe 'find-file-in-project))
(global-set-key (kbd "M-s-p") (with-safe 'find-file-in-current-directory))
(global-set-key (kbd "C-s-m") 'counsel-evil-goto-global-marker)



;;----------------------------------------------------------------------------
;; Buffer and windows
;;----------------------------------------------------------------------------
;;(global-set-key (kbd "C-s") 'save-buffer)            ;; save
(global-set-key (kbd "s-w") 'kill-this-buffer)       ;; close의 의미
(global-set-key (kbd "C-`") 'other-window-treemacs)       ;; Switch window
(global-set-key (kbd "C-s-`") 'prev-window)       ;; Switch window
(global-set-key (kbd "C-~") 'copy-buffers-in-windows) ;; copy-buffers-in-windows
(global-set-key (kbd "C-s-~") 'copy-buffers-in-prev-windows) ;; copy-buffers-in-prev-windows
(global-set-key (kbd "s-b") 'ibuffer)            ;; list buffer
(global-set-key (kbd "s-t")  'new-empty-buffer)
(global-set-key (kbd "M-t")  'treemacs)
(global-set-key (kbd "M-s-t")  'treemacs-add-project)
(global-set-key (kbd "C-c \`")  'window-swap-states)
(global-set-key (kbd "<C-S-tab>") 'switch-to-last-buffer)
(global-set-key (kbd "<C-tab>") 'quit-window)
(global-set-key (kbd "M-z") 'writeroom-mode)
(global-set-key (kbd "s-g") 'counsel-imenu)
(global-set-key (kbd "C-s-g") 'ivy-imenu-anywhere)
(global-set-key (kbd "M-s-w") 'delete-frame)
(global-set-key (kbd "s-n") 'new-frame-and-maximize)

;;(global-set-key (kbd "s-{") 'awesome-tab-backward-tab)
;;(global-set-key (kbd "s-}") 'awesome-tab-forward-tab)
;;(global-set-key (kbd "C-s-{") 'awesome-tab-backward-group)
;;(global-set-key (kbd "C-s-}") 'awesome-tab-forward-group)

(global-set-key (kbd "s-{") 'tabbar-backward-tab)
(global-set-key (kbd "s-}") 'tabbar-forward-tab)
(global-set-key (kbd "C-s-{") 'tabbar-backward-group)
(global-set-key (kbd "C-s-}") 'tabbar-forward-group)

(global-set-key (kbd "C-c d") 'xref-find-definitions)
(global-set-key (kbd "C-c s") 'xref-pop-marker-stack)
(global-set-key (kbd "C-c C-p") 'counsel-open-project)

;; overrides
(require 'json-mode)
(define-key json-mode-map (kbd "C-c C-p") 'counsel-open-project)

;;----------------------------------------------------------------------------
;; Evil
;;----------------------------------------------------------------------------
(require 'evil)

(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-normal-state-map (kbd "SPC") 'avy-goto-word-1)
(define-key evil-normal-state-map (kbd "C-SPC") 'avy-goto-word-1)
(define-key evil-normal-state-map (kbd "TAB") 'evil-jump-backward)
(define-key evil-normal-state-map (kbd "g s") 'magit-status)
(define-key evil-normal-state-map (kbd "g a") 'magit-stage-file)
(define-key evil-normal-state-map (kbd "g c") 'magit-commit-current-file)
(define-key evil-normal-state-map (kbd "g b") 'magit-blame-echo)
(define-key evil-normal-state-map (kbd "s-2") 'flycheck-next-error)
(define-key evil-normal-state-map (kbd "s-@") 'flycheck-previous-error)
(define-key evil-normal-state-map (kbd "C-!") 'counsel-flycheck)
(define-key evil-normal-state-map (kbd "?") 'tide-hl-identifier) ;; create custom function later
(define-key evil-inner-text-objects-map "b" 'evil-textobj-anyblock-inner-block)
(define-key evil-outer-text-objects-map "b" 'evil-textobj-anyblock-a-block)
(define-key evil-normal-state-map "\C-n" nil)
(define-key evil-normal-state-map "\C-t" nil)
(define-key evil-normal-state-map "\C-e" nil)
(define-key evil-normal-state-map "\C-z" nil)
(define-key evil-motion-state-map "\C-w" nil)
(define-key evil-motion-state-map "\C-b" nil)
(define-key evil-motion-state-map "\C-e" nil)
(define-key evil-motion-state-map "\C-f" nil)
(define-key evil-motion-state-map "\C-o" nil)
(define-key evil-motion-state-map "\C-z" nil)


;;----------------------------------------------------------------------------
;; Ivy
;;----------------------------------------------------------------------------
(require 'ivy)
(define-key evil-normal-state-map "\C-p" 'ivy-yasnippet)
(define-key evil-insert-state-map "\C-p" 'ivy-yasnippet)
(define-key evil-insert-state-map "\C-p" 'ivy-yasnippet)
(define-key ivy-occur-mode-map (kbd "<return>") 'ivy-occur-press)


;;----------------------------------------------------------------------------
;; Company
;;----------------------------------------------------------------------------
(require 'company)
(define-key company-active-map (kbd "<tab>") 'yas/expand)
(define-key company-active-map (kbd "?") #'company-quickhelp-manual-begin)
(define-key company-search-map (kbd "<tab>") 'yas/expand)
(define-key company-active-map (kbd "<escape>") 'company-abort)
(define-key company-active-map (kbd "C-o") 'counsel-company)

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
(global-set-key (kbd "s-1") 'open-til)
(global-set-key (kbd "C-s-1") 'open-todo)


;;----------------------------------------------------------------------------
;; Unset
;;----------------------------------------------------------------------------
(global-unset-key "\C-z")
(global-unset-key "\C-t")

;;----------------------------------------------------------------------------
;; Typescript
;;----------------------------------------------------------------------------
(require 'tide)
(define-key tide-mode-map (kbd "C-c r") 'tide-references)
(define-key tide-mode-map (kbd "M-s-r") 'tide-restart-server)
(define-key tide-mode-map (kbd "C-c f") 'tide-refactor)
(define-key tide-mode-map (kbd "C-c d") 'tide-jump-to-definition)
(define-key tide-mode-map (kbd "C-c s") 'xref-pop-marker-stack)
(define-key tide-mode-map (kbd "C-c C-r") 'tide-rename-symbol)
(define-key tide-mode-map (kbd "C-c C-f") 'tide-fix)
(define-key tide-mode-map (kbd "C-c C-e") 'tide-project-errors)
(define-key tide-mode-map (kbd "C-c h") 'tide-documentation-at-point)
(define-key tide-mode-map (kbd "s-s") 'safe-invoke-prettier)
(define-key tide-mode-map (kbd "s-i") 'node/import)
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
;; (define-key web-mode-map (kbd "C-c C-d") 'web-mode-attribute-kill)
;; (define-key web-mode-map (kbd "C-c f") 'fold-active-region)

(define-key evil-visual-state-map (kbd "v") 'er/expand-region)        ;; expand region plugin
;;(global-set-key (kbd "s-]") 'er/expand-region)        ;; expand region plugin

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
(define-key rustic-mode-map (kbd "C-c d") 'lsp-find-definition)
(define-key rustic-mode-map (kbd "C-c r") 'lsp-find-references)
(define-key rustic-mode-map (kbd "C-c h") 'lsp-describe-thing-at-point)
(define-key rustic-mode-map (kbd "C-c C-f") 'lsp-execute-code-action)
(define-key rustic-mode-map (kbd "C-c C-p") 'counsel-open-project)

;;----------------------------------------------------------------------------
;; slime
;;----------------------------------------------------------------------------
(require 'slime)
(define-key lisp-mode-map (kbd "C-c d") 'slime-edit-definition)
(define-key lisp-mode-map (kbd "C-c r") 'slime-who-calls)

;;----------------------------------------------------------------------------
;; node-bindings
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-c C-o") 'node/counsel-open)


;;----------------------------------------------------------------------------
;; markdown-bindings
;;----------------------------------------------------------------------------
(require 'markdown-mode)
(define-key markdown-mode-map (kbd "C-c C-p") 'counsel-open-project)
(define-key markdown-mode-map (kbd "C-c d") 'markdown-jump)
(define-key markdown-mode-map (kbd "s-i") 'ffip-find-relative-path)

(provide 'init-bindings)
