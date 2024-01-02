;;; init-org-roam.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; 
;;;; Requirements:
(require 'org-roam)

;;; Code:
(setq org-roam-directory (file-truename "~/org"))
(setq find-file-visit-truename t)
(org-roam-db-autosync-mode)

(message "init-org-roam.el")
(provide 'init-org-roam)
;;; init-org-roam.el ends here
