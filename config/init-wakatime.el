;;; init-wakatime.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'wakatime-mode)
  (global-wakatime-mode)
  (let ((api-key
          (f-read-text
            (substitute-in-file-name (expand-file-name "~/.credentials/wakatime-api-key"))
            'utf-8)))
    (setq wakatime-api-key api-key))
  
  (setq wakatime-cli-path "/usr/local/bin/wakatime"))

(defun waka ()
  "Open wakatime-dashboard in browser."
  (interactive)
  (browse-url "https://wakatime.com/dashboard"))

(provide 'init-wakatime)
;;; init-wakatime.el ends here
