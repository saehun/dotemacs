;;; init-copilot.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :config
  (setq copilot-node-executable "/usr/local/n/versions/node/16.17.1/bin/node"))

(provide 'init-copilot)
;;; init-copilot.el ends here
