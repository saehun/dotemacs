;;; init-chatgpt-shell.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; 
;;;; Requirements:
(require 'ob-chatgpt-shell)
(require 'chatgpt-shell)

;;; Code:
(ob-chatgpt-shell-setup)
(setq chatgpt-shell-model-version "gpt-3.5-turbo")

(message "init-chatgpt-shell.el")
(provide 'init-chatgpt-shell)
;;; init-chatgpt-shell.el ends here
