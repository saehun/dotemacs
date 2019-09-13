

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
(global-set-key (kbd "s-w") 'kill-buffer-and-return-previous)       ;; close의 의미
(global-set-key (kbd "C-`") 'other-window)       ;; Switch window
(global-set-key (kbd "C-~") 'copy-buffers-in-windows) ;; copy-buffers-in-windows
(global-set-key (kbd "s-b") 'ibuffer)            ;; list buffer
(global-set-key (kbd "s-t")  'new-empty-buffer)
(global-set-key (kbd "C-c \`")  'window-swap-states)


;;----------------------------------------------------------------------------
;; Misc
;;----------------------------------------------------------------------------
(global-set-key (kbd "s-r") 'shell-command)          ;; run command


;; (global-set-key (kbd "M-r") 'query-replace)          ;; replace
;; (global-set-key (kbd "s-[") 'er/contract-region)        ;; expand region plugin
;; (global-set-key (kbd "s-]") 'er/expand-region)        ;; expand region plugin
;; (global-set-key (kbd "s-/") 'comment-line)        ;; comment
;; (global-set-key (kbd "C-x o") 'open-file-at-cursor)        ;; open file under the cursor
;; (global-set-key (kbd "C-c l") 'goto-last-change)
;; (global-set-key (kbd "C-c p") 'yas/expand)
;; (global-set-key (kbd "C-SPC") 'company-complete)
;; (global-set-key (kbd "S-SPC") 'toggle-input-method)
;; (global-set-key (kbd "s-2") 'jump-to-cursor)
;; (global-set-key (kbd "C-c r") 'tide-references)
;; (global-set-key (kbd "C-c d") 'tide-jump-to-definition)
;; (define-key dired-mode-map (kbd "y") 'dired-ranger-copy)
;; (define-key dired-mode-map (kbd "p") 'dired-ranger-paste)
;; (global-set-key (kbd "s-<left>") 'move-beginning-of-line)
;; (global-set-key (kbd "s-<right>") 'move-end-of-line)

(provide 'init-bindings)

