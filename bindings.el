

;;----------------------------------------------------------------------------
;; tabbar
;;----------------------------------------------------------------------------
(define-key global-map (kbd "s-{") 'tabbar-backward-tab)
(define-key global-map (kbd "s-}") 'tabbar-forward-tab)
(global-set-key (kbd "<C-tab>") 'tabbar-forward-group)      
(global-set-key (kbd "<C-S-tab>") 'tabbar-backward-group)   
