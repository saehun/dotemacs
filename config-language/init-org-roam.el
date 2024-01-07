;;; init-org-roam.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; 
;;;; Requirements:
(require 'org-roam)
(require 'consult-org-roam)
(require 'delve)

;;; Code:
(setq org-roam-directory (file-truename "~/org"))
(setq org-roam-node-display-template
      (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

(setq find-file-visit-truename t)

;; Delve
;; https://github.com/publicimageltd/delve
(setq delve-storage-paths "~/org")

;; Consult-org-roam
;; https://github.com/jgru/consult-org-roam
(consult-org-roam-mode 1)
(customize-set-variable 'consult-org-roam-grep-func #'consult-ripgrep)  ; Better syntax highlighting

(org-roam-db-autosync-mode)

(message "init-org-roam.el")
(provide 'init-org-roam)
;;; init-org-roam.el ends here
