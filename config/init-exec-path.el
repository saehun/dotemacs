;;; init-exec-path.el --- Set up exec-path to help Emacs find programs  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'exec-path-from-shell)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setenv "PATH" (concat (getenv "PATH") ":/Users/minidonut/bin"))
(setq exec-path (append exec-path '("/Users/minidonut/bin")))
(setenv "PATH" (concat (getenv "PATH") ":/Users/minidonut/.local/bin"))
(setq exec-path (append exec-path '("/Users/minidonut/.local/bin")))


(provide 'init-exec-path)
;;; init-exec-path.el ends here
