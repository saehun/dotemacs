;;; package --- init-bindings
;;; Commentary:
;;; Code:
(require 'yasnippet)
(require 'org)
(require 'org-roam)


(defun with-safe (command)
  "Check whether minibuffer opened, running COMMAND, close seamlessly."
  `(lambda (&rest args)
     (interactive)
     (if (minibuffer-window-active-p (active-minibuffer-window))
         (minibuffer-keyboard-quit)
       (apply #',command args))))

;;----------------------------------------------------------------------------
;; Unset
;;----------------------------------------------------------------------------
(global-unset-key "\C-z")
(global-unset-key "\C-t")
(global-unset-key "\C-i")
(global-unset-key "\C-r")
(global-unset-key "\C-o")
(global-set-key (kbd "s-r") nil)
(global-unset-key (kbd "s-r"))
(global-unset-key (kbd "s-e"))
(global-set-key (kbd "s-e") nil)

;;----------------------------------------------------------------------------
;; Basic
;;----------------------------------------------------------------------------
;;(define-key function-key-map [tab] ["TAB"])

(global-set-key (kbd "s-e") (with-safe 'counsel-M-x))
(global-set-key (kbd "s-f") 'swiper)
(global-set-key (kbd "s-F") (with-safe 'counsel-rg))         ;; grep from git root
(global-set-key (kbd "C-f") (with-safe 'counsel-rg-package)) ;; grep from package.json root
(global-set-key (kbd "M-s-F") (with-safe 'counsel-rg-here))  ;; grep from here
(global-set-key (kbd "s-G") (with-safe 'counsel-google))
(global-set-key (kbd "C-b") (with-safe 'counsel-switch-buffer))
(global-set-key (kbd "C-n") (with-safe 'counsel-find-file))
(global-set-key (kbd "s-p") (with-safe 'find-file-in-project))
(global-set-key (kbd "M-p") (with-safe 'projectile-find-file))
(global-set-key (kbd "M-s-p") (with-safe 'find-file-in-current-directory))
(global-set-key (kbd "C-s-m") 'counsel-evil-goto-global-marker)
(global-set-key (kbd "C-s-r") 'shell-command)
(global-set-key (kbd "C-c C-c") 'open-cursor-editor)


;;----------------------------------------------------------------------------
;; Buffer and windows
;;----------------------------------------------------------------------------
(global-set-key (kbd "s-w") 'kill-this-buffer)       ;; close의 의미
(global-set-key (kbd "C-`") 'other-window-treemacs)       ;; Switch window
(global-set-key (kbd "C-M-`") 'prev-window)       ;; Switch window
(global-set-key (kbd "C-~") 'copy-buffers-in-windows) ;; copy-buffers-in-windows
(global-set-key (kbd "C-M-~") 'copy-buffers-in-prev-windows) ;; copy-buffers-in-prev-windows
(global-set-key (kbd "s-b") 'ibuffer)            ;; list buffer
(global-set-key (kbd "s-t")  'new-empty-buffer)
(global-set-key (kbd "M-t")  'treemacs-smart)
(global-set-key (kbd "M-s-t")  'treemacs-add-project)
(global-set-key (kbd "C-c \`")  'window-swap-states)
(global-set-key (kbd "<C-S-tab>") 'switch-to-last-buffer)
(global-set-key (kbd "<C-tab>") 'quit-window)
(global-set-key (kbd "M-z") 'writeroom-mode)
(global-set-key (kbd "s-g") 'counsel-imenu)
(global-set-key (kbd "C-s-g") 'ivy-imenu-anywhere)
(global-set-key (kbd "M-s-w") 'delete-frame)
(global-set-key (kbd "M-s-n") 'new-frame-and-maximize)

(require 'treemacs)
(define-key treemacs-mode-map (kbd "s-w") 'treemacs-remove-project-from-workspace)
(define-key treemacs-mode-map (kbd "s-r") 'treemacs-refresh)

(global-set-key (kbd "s-{") 'tabbar-backward-tab)
(global-set-key (kbd "s-}") 'tabbar-forward-tab)
(global-set-key (kbd "C-s-{") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "C-s-}") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "s-0") 'tab-bar-select-first)

(global-set-key (kbd "C-c d") 'xref-find-definitions)
(global-set-key (kbd "C-c s") 'xref-go-back)
(global-set-key (kbd "C-o C-p") 'counsel-open-project)

;; overrides
(require 'json-mode)
(define-key json-mode-map (kbd "C-o C-p") 'counsel-open-project)

;;----------------------------------------------------------------------------
;; Evil
;;----------------------------------------------------------------------------
(require 'evil)

(define-key evil-normal-state-map (kbd "SPC") 'avy-goto-char-timer)
;;(define-key evil-normal-state-map (kbd "TAB") 'evil-jump-backward)
(define-key evil-normal-state-map (kbd "g s") 'magit-status)
(define-key evil-normal-state-map (kbd "g a") 'magit-stage-file)
(define-key evil-normal-state-map (kbd "g c") 'magit-commit-current-file)
(define-key evil-normal-state-map (kbd "g b") 'magit-blame-echo)
(define-key evil-normal-state-map (kbd "s-2") 'flycheck-next-error)
(define-key evil-normal-state-map (kbd "M-s-2") 'flycheck-previous-error)
(define-key evil-normal-state-map (kbd "C-!") 'counsel-flycheck)
(define-key evil-inner-text-objects-map "b" 'evil-textobj-anyblock-inner-block)
(define-key evil-outer-text-objects-map "b" 'evil-textobj-anyblock-a-block)
(define-key evil-visual-state-map "\C-r" nil)
(define-key evil-normal-state-map "\C-r" nil)
(define-key evil-visual-state-map "L" 'insert-log-with-kill-ring-or-region)
(define-key evil-normal-state-map "L" 'insert-log-with-kill-ring-or-region)
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

(define-key evil-normal-state-map "\C-i" nil)
(define-key evil-visual-state-map "\C-i" nil)
(define-key evil-insert-state-map "\C-i" nil)
(define-key evil-motion-state-map "\C-i" nil)

;; custom function bound to normal-state "?"
(defun evil-normal-state-question-mark ()
  "Execute some functionality with evil normal state question mark."
  (interactive)
  (cond
   ((eq major-mode 'org-mode)
    (progn (org-toggle-link-display) (org-toggle-inline-images)))
   ((bound-and-true-p tide-mode) (tide-hl-identifier))
   (t (message "Not supported ?"))))

(define-key evil-normal-state-map (kbd "?") 'evil-normal-state-question-mark)

;;----------------------------------------------------------------------------
;; Ivy
;;----------------------------------------------------------------------------
(require 'ivy)
(define-key evil-normal-state-map "\C-p" 'ivy-yasnippet)
(define-key evil-insert-state-map "\C-p" 'ivy-yasnippet)
(define-key evil-insert-state-map "\C-p" 'ivy-yasnippet)
(define-key ivy-occur-mode-map (kbd "9") 'ivy-occur-press)


;;----------------------------------------------------------------------------
;; Company
;;----------------------------------------------------------------------------
(require 'company)
(define-key company-active-map (kbd "<tab>") yas-maybe-expand)
(define-key company-active-map (kbd "?") #'company-quickhelp-manual-begin)
(define-key company-search-map (kbd "<tab>") yas-maybe-expand)
(define-key company-active-map (kbd "<escape>") 'company-abort)
(define-key company-active-map (kbd "C-o") 'counsel-company)

(global-set-key (kbd "<s-up>") 'previous-3-line)
(global-set-key (kbd "<s-down>") 'next-3-line)
(global-set-key (kbd "<s-right>") 'forward-word)
(global-set-key (kbd "<s-left>") 'backward-word)

;;----------------------------------------------------------------------------
;; Edit
;;----------------------------------------------------------------------------
(global-set-key (kbd "<f17>") 'toggle-input-method)
(define-key yas-minor-mode-map (kbd "<tab>") yas-maybe-expand)
(define-key evil-normal-state-map (kbd "s-/") 'comment-line)
(define-key evil-insert-state-map (kbd "s-/") 'comment-line)
(define-key evil-visual-state-map (kbd "s-/") 'comment-or-uncomment-region)

;; ----------------------------------------------------------------------------
;; Misc
;;----------------------------------------------------------------------------
(global-set-key (kbd "s-r") 'shell-command)          ;; run command
(global-set-key (kbd "C-SPC") 'company-complete)
(global-set-key (kbd "C-x C-e") 'counsel-shell-command)
(global-set-key (kbd "s-1") 'open-til)
(global-set-key (kbd "s-<f1>") 'open-daily)
(global-set-key (kbd "C-s-1") 'open-todo)
(global-set-key (kbd "C-o C-g") 'dired-git-root)
(global-set-key (kbd "C-o C-d") 'org-roam-dailies-goto-today)
(global-set-key (kbd "C-ㅐ C-ㅇ") 'org-roam-dailies-goto-today)
(global-set-key (kbd "C-o C-o") 'org-roam-node-find)
(global-set-key (kbd "C-ㅐ C-ㅐ") 'org-roam-node-find)


;;----------------------------------------------------------------------------
;; Typescript
;;----------------------------------------------------------------------------
(require 'tide)
(define-key tide-mode-map (kbd "C-c r") 'tide-references)
(define-key tide-mode-map (kbd "M-s-r") 'tide-restart-server)
(define-key tide-mode-map (kbd "C-c f") 'my-tide-refactor)
(define-key tide-mode-map (kbd "C-r") 'my-tide-refactor)
(define-key tide-mode-map (kbd "C-c d") 'tide-jump-to-definition)
(define-key tide-mode-map (kbd "C-c i") 'tide-jump-to-implementation)
(define-key tide-mode-map (kbd "C-c t") 'node/new-test)
(define-key tide-mode-map (kbd "C-c s") 'xref-go-back)
(define-key tide-mode-map (kbd "C-c C-r") 'tide-rename-symbol)
(define-key tide-mode-map (kbd "C-c C-f") 'my-tide-fix)
(define-key tide-mode-map (kbd "C-c C-e") 'tide-project-errors)
(define-key tide-mode-map (kbd "C-c h") 'tide-documentation-at-point)
;; (define-key tide-mode-map (kbd "s-s") 'safe-invoke-prettier)
(define-key tide-mode-map (kbd "s-i") 'node/import)
(define-key tide-mode-map (kbd "C-t") 'node/run-test)
(define-key tide-mode-map (kbd "C-M-t") 'node/run-test-debug)
(define-key tide-references-mode-map (kbd "<return>") 'tide-goto-line-reference)

;;----------------------------------------------------------------------------
;; Web-mode
;;----------------------------------------------------------------------------
(require 'web-mode)
(define-key web-mode-map (kbd "C-c C-a") 'web-mode-attribute-select)
(define-key web-mode-map (kbd "C-c C-c") 'web-mode-element-content-select)
(define-key web-mode-map (kbd "C-c C-t") 'web-mode-tag-select)
(define-key web-mode-map (kbd "C-c C-e") 'web-mode-element-select)
(define-key web-mode-map (kbd "C-c C-w") 'web-mode-tag-end)

(define-key evil-visual-state-map (kbd "v") 'er/expand-region)        ;; expand region plugin
(define-key evil-visual-state-map (kbd "(") 'wrap-region-parenthesis-to-function)


(require 'magit)
(define-key magit-mode-map (kbd "<down>") 'magit-section-forward)
(define-key magit-mode-map (kbd "<up>") 'magit-section-backward)
(define-key magit-mode-map (kbd "<C-tab>") 'quit-window)
(global-set-key (kbd "<s-backspace>") 'go-up-general)

;;----------------------------------------------------------------------------
;; node-bindings
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-c C-o") 'node/counsel-open)


;;----------------------------------------------------------------------------
;; markdown-bindings
;;----------------------------------------------------------------------------
(require 'markdown-mode)
(define-key markdown-mode-map (kbd "C-o C-p") 'counsel-open-project)
(define-key markdown-mode-map (kbd "C-c d") 'markdown-jump)
(define-key markdown-mode-map (kbd "s-i") 'ffip-insert-relative-path)

;;----------------------------------------------------------------------------
;; I don't have a clue why binding doen't work at the top of the code.
;;----------------------------------------------------------------------------
(global-set-key (kbd "s-r") 'revert-buffer-no-confirm)
(global-set-key (kbd "C-/") 'iterm)
(global-set-key (kbd "C-s-/") 'iterm-here)

;;----------------------------------------------------------------------------
;; Wdired
;;----------------------------------------------------------------------------
(require 'wdired)
(define-key dired-mode-map (kbd "C-c C-c") 'wdired-change-to-wdired-mode)
(define-key wdired-mode-map (kbd "C-c C-c") 'wdired-finish-edit)
(define-key wdired-mode-map (kbd "C-c C-q") 'wdired-abort-changes)

(define-key dired-mode-map (kbd "C-o") nil)
(define-key dired-mode-map (kbd "o") nil)
(define-key dired-mode-map (kbd "C-<return>") 'dired-find-file-other-window)
(define-key dired-mode-map (kbd "C-o C-p") 'counsel-open-project)

(global-set-key (kbd "C-o C-r") 'consult-recent-file)
(global-set-key (kbd "C-o r") 'projectile-recentf)

;;----------------------------------------------------------------------------
;; Korean binding
;;----------------------------------------------------------------------------
;; 한글 자모와 대응하는 영어 코드 배열 정의
(let ((hangul-to-english
       '(("ㄱ" . "r") ("ㄴ" . "s") ("ㄷ" . "e") ("ㄹ" . "f")
         ("ㅁ" . "a") ("ㅂ" . "q") ("ㅅ" . "t") ("ㅇ" . "d")
         ("ㅈ" . "w") ("ㅊ" . "c") ("ㅋ" . "z") ("ㅌ" . "x")
         ("ㅍ" . "v") ("ㅎ" . "g") ("ㅏ" . "k") ("ㅐ" . "o")
         ("ㅑ" . "i") ("ㅓ" . "j") ("ㅔ" . "p") ("ㅕ" . "u")
         ("ㅗ" . "h") ("ㅛ" . "y") ("ㅜ" . "n") ("ㅠ" . "b")
         ("ㅡ" . "m") ("ㅣ" . "l")
         ;; 추가적인 기본 모음과 자음을 여기에 넣을 수 있습니다
         )))
  (dolist (pair hangul-to-english)
    (let ((hangul (car pair))
          (english (cdr pair)))
      ;; Control과 Shift 조합에 대한 키맵 정의
      (define-key key-translation-map (kbd (concat "C-" hangul)) (kbd (concat "C-" english)))
      (define-key key-translation-map (kbd (concat "s-" hangul)) (kbd (concat "s-" english)))
      (define-key key-translation-map (kbd (concat "M-" hangul)) (kbd (concat "M-" english)))
      ;; 여기에 추가적인 키 조합을 넣을 수 있습니다
      )))

(provide 'init-bindings)
