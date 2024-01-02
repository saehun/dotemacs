;;; init-org.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; 
;;;; Requirements:

;;; brew install gimp
;;; brew install pngpaste
;;; brew install pandoc
(require 'typescript-ts-mode)
(require 'rustic)
(require 'yasnippet)
(require 'org)
(require 'ob-js)
(require 'ob-rust)
(require 'ob-typescript)
(require 'org-footnote-assistant)
(require 'org-bullets)
(require 'org-download)
(require 'org-appear)
(require 'org-compat)
(require 'general)
(require 'hydra)


;;; Code
(defun yas-org-very-safe-expand ()
  "Safe expand."
  (let ((yas-fallback-behavior 'return-nil)) (yas-expand)))

;; C-c C-c without yes
(setq org-startup-indented t)
(setq org-confirm-babel-evaluate nil)
(setq org-download-image-org-width 500)
(setq org-startup-with-inline-images t)
(setq org-hide-emphasis-markers t)
(setq org-preview-latex-default-process 'dvisvgm)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

(org-footnote-assistant-mode 1)


;; babel setting
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (typescript . t)
   (python . t)
   (shell . t)
   (rust . t)
   (js . t)
   ))

(setq org-src-lang-modes
      (append org-src-lang-modes
              '(
                ("typescript" . typescript-ts)
                ("rust" . rustic)
                ("js" . js-ts)
                )))

(setq org-babel-js-function-wrapper "%s")

(defun org-insert-now-timestamp ()
  "Insert org mode timestamp at point with current date and time."
  (interactive)
  (org-insert-time-stamp (current-time) t))

;; yasnippet TAB support
(add-hook 'org-mode-hook
          (lambda ()
            (add-to-list 'org-tab-first-hook 'yas-org-very-safe-expand)
            (define-key yas-keymap [tab] 'yas-next-field)))

(add-hook 'org-mode-hook 'org-appear-mode)
(add-hook 'org-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; key bindings
(define-key org-mode-map (kbd "C-c <right>") 'org-insert-link)
(define-key org-mode-map (kbd "C-c d") 'org-open-at-point)
(define-key org-mode-map (kbd "C-c s") 'org-mark-ring-goto)
(define-key org-mode-map (kbd "C-c t") 'org-insert-now-timestamp)

;; (general-define-key
;;  :states 'normal
;;  :keymaps 'org-mode-map
;;  "<RET>" 'elisp-slime-nav-describe-elisp-thing-at-point)

(message "init-org.el")
(provide 'init-org)
;;; init-org.el ends here
