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

;; https://github.com/nobiot/org-transclusion <- see it later

;;; Code
(defun yas-org-very-safe-expand ()
  "Safe expand."
  (let ((yas-fallback-behavior 'return-nil)) (yas-expand)))

;; C-c C-c without yes
(setq org-startup-indented t)
(setq org-hide-drawer-startup t)
(setq org-confirm-babel-evaluate nil)
(setq org-download-image-org-width 500)
(setq org-startup-with-inline-images t)
(setq org-hide-emphasis-markers t)
(setq org-preview-latex-default-process 'dvisvgm)
(setq org-id-link-to-org-use-id 'use-existing)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

(org-footnote-assistant-mode 1)


;; babel setting
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (restclient . t)
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
                ("Dockerfile" . dockerfile-ts)
                )))

(setq org-babel-js-function-wrapper "%s")

(defun org-insert-now-timestamp ()
  "Insert org mode timestamp at point with current date and time."
  (interactive)
  (org-insert-time-stamp (current-time) t))

(defun org-update-updated-at-property ()
  "Update the UPDATED_AT property in the file-level PROPERTIES drawer."
  (save-excursion
    (goto-char (point-min))
    ;; Check if there's a PROPERTIES drawer at the beginning
    (when (looking-at-p "^:PROPERTIES:")
      (let ((end (save-excursion
                   (re-search-forward "^:END:" nil t))))
        (when end
          ;; Look for existing UPDATED_AT
          (if (re-search-forward "^:UPDATED_AT:.*$" end t)
              ;; Replace existing
              (replace-match 
               (format ":UPDATED_AT: [%s]" 
                       (format-time-string "%Y-%m-%d %a %H:%M:%S"))
               t t)
            ;; Add new before :END:
            (goto-char end)
            (beginning-of-line)
            (insert (format ":UPDATED_AT: [%s]\n" 
                            (format-time-string "%Y-%m-%d %a %H:%M:%S")))))))))

(defun org-add-created-at-property ()
  "Add CREATED_AT property if it doesn't exist in the file-level PROPERTIES drawer."
  (save-excursion
    (goto-char (point-min))
    ;; Check if there's a PROPERTIES drawer at the beginning
    (when (looking-at-p "^:PROPERTIES:")
      (let ((end (save-excursion
                   (re-search-forward "^:END:" nil t))))
        (when end
          ;; Check if CREATED_AT already exists
          (goto-char (point-min))
          (unless (re-search-forward "^:CREATED_AT:.*$" end t)
            ;; Add CREATED_AT before :END:
            (goto-char end)
            (beginning-of-line)
            (insert (format ":CREATED_AT: [%s]\n" 
                            (format-time-string "%Y-%m-%d %a %H:%M:%S")))))))))

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

;; yasnippet TAB support
(add-hook 'org-mode-hook
          (lambda ()
            (add-to-list 'org-tab-first-hook 'yas-org-very-safe-expand)
            (define-key yas-keymap [tab] 'yas-next-field)))

(add-hook 'org-mode-hook 'org-appear-mode)
(add-hook 'org-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-mode-hook (lambda () (setq-local fill-column 90)))
(add-hook 'org-mode-hook 
          (lambda ()
            (add-hook 'before-save-hook 
                      (lambda ()
                        (org-add-created-at-property)
                        (org-update-updated-at-property))
                      nil t)))

;; key bindings
(define-key org-mode-map (kbd "C-c <right>") 'org-insert-link)
(define-key org-mode-map (kbd "C-c d") 'org-open-at-point)
(define-key org-mode-map (kbd "C-c s") 'org-mark-ring-goto)
(define-key org-mode-map (kbd "C-c t") 'org-insert-now-timestamp)
(define-key org-mode-map (kbd "C-c C-n") 'org-roam-node-insert-immediate)
(define-key org-mode-map (kbd "C-c C-p") 'org-web-tools-insert-link-for-url)
(define-key org-mode-map (kbd "C-o C-t") 'org-roam-tag-add)
(define-key org-mode-map (kbd "C-f") 'consult-org-heading)

;; Apperances
(defface org-link-http
  '((t :foreground "#33a14f"
       :weight bold
       :underline t))
  "Face for Org-Mode links starting with http:."
  :group 'org-faces)
(org-link-set-parameters
 "http"
 :face 'org-link-http)
(defface org-link-https
  '((t :foreground "#33a14f"
       :weight bold
       :underline t))
  "Face for Org-Mode links starting with https:."
  :group 'org-faces)
(org-link-set-parameters
 "https"
 :face 'org-link-https)
(custom-set-faces
 '(org-document-title ((t (:foreground "gray90" :weight bold :height 1.2))))
 '(org-drawer ((t (:foreground "gray40")))))

;; (general-define-key
;;  :states 'normal
;;  :keymaps 'org-mode-map
;;  "<RET>" 'elisp-slime-nav-describe-elisp-thing-at-point)

(message "init-org.el")
(provide 'init-org)
;;; init-org.el ends here
