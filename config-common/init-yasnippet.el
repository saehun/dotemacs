;;; init-yasnippet.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; 
;;;; Requirements:
(require 'yasnippet)
(require 'ivy-yasnippet)
(require 'string-inflection)

;;; Code:
(yas-global-mode 1)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

(string-inflection-all-cycle)

(message "init-yasnippet.el")
(provide 'init-yasnippet)
;;; init-yasnippet.el ends here

