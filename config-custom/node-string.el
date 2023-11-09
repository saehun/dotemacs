;;; package --- node-string
;;; Commentary:
;;; Code:

(defun node-string/json-to-yaml (&optional b e)
  "Json to Yaml."
  (interactive "r")
  (call-process-region b e "emacs-node-string" t t nil "json-to-yaml"))

(defun node-string/object-to-json (&optional b e)
  "Object to Json."
  (interactive "r")
  (call-process-region b e "emacs-node-string" t t nil "object-to-json"))

(defun node-string/querystring-to-json (&optional b e)
  "Querystring to Json."
  (interactive "r")
  (call-process-region b e "emacs-node-string" t t nil "querystring-to-json"))

(defun node-string/yaml-to-json (&optional b e)
  "Yaml to Json."
  (interactive "r")
  (call-process-region b e "emacs-node-string" t t nil "yaml-to-json"))


(provide 'node-string)
;;; node-string.el ends here

