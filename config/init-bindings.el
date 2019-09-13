

;;----------------------------------------------------------------------------
;; tabbar
;;----------------------------------------------------------------------------
(define-key global-map (kbd "s-{") 'tabbar-backward-tab)
(define-key global-map (kbd "s-}") 'tabbar-forward-tab)
(global-set-key (kbd "<C-tab>") 'tabbar-forward-group)      
(global-set-key (kbd "<C-S-tab>") 'tabbar-backward-group)   

;;----------------------------------------------------------------------------
;; counsel
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-e") 'counsel-M-x)
(global-set-key (kbd "s-F") 'counsel-rg)
(global-set-key (kbd "C-b") 'counsel-ibuffer) 
(global-set-key (kbd "C-n") 'find-file)
(global-set-key (kbd "s-f") 'swiper)


;;----------------------------------------------------------------------------
;; Buffer and windows
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-s") 'save-buffer)            ;; save
(global-set-key (kbd "s-w") 'kill-current-buffer)       ;; close의 의미
(global-set-key (kbd "C-`") 'other-window)       ;; Switch window
(global-set-key (kbd "C-~") 'copy-buffers-in-windows) ;; copy-buffers-in-windows
(global-set-key (kbd "s-b") 'ibuffer)            ;; list buffer
(global-set-key (kbd "s-t")  'new-empty-buffer)
(global-set-key (kbd "C-c \`")  'window-swap-states)


;;----------------------------------------------------------------------------
;; Evil
;;----------------------------------------------------------------------------
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-word-mode)
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


;;----------------------------------------------------------------------------
;; Edit
;;----------------------------------------------------------------------------
(global-set-key (kbd "S-SPC") 'toggle-input-method)

;;----------------------------------------------------------------------------
;; Misc
;;----------------------------------------------------------------------------
(global-set-key (kbd "s-r") 'shell-command)          ;; run command


;;----------------------------------------------------------------------------
;; Unset
;;----------------------------------------------------------------------------
(global-unset-key "\C-z")


;; (global-set-key (kbd "M-r") 'query-replace)          ;; replace
;; (global-set-key (kbd "s-[") 'er/contract-region)        ;; expand region plugin
;; (global-set-key (kbd "s-]") 'er/expand-region)        ;; expand region plugin
;; (global-set-key (kbd "s-/") 'comment-line)        ;; comment
;; (global-set-key (kbd "C-x o") 'open-file-at-cursor)        ;; open file under the cursor
;; (global-set-key (kbd "C-c l") 'goto-last-change)
;; (global-set-key (kbd "C-c p") 'yas/expand)
;; (global-set-key (kbd "C-SPC") 'company-complete)
;; (global-set-key (kbd "s-2") 'jump-to-cursor)
;; (global-set-key (kbd "C-c r") 'tide-references)
;; (global-set-key (kbd "C-c d") 'tide-jump-to-definition)
;; (define-key dired-mode-map (kbd "y") 'dired-ranger-copy)
;; (define-key dired-mode-map (kbd "p") 'dired-ranger-paste)
;; (global-set-key (kbd "s-<left>") 'move-beginning-of-line)
;; (global-set-key (kbd "s-<right>") 'move-end-of-line)

(provide 'init-bindings)

