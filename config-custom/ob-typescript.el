;;; ob-typescript.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; 
;;;; Requirements:
(require 'ob)

;;; Code:
(add-to-list 'org-babel-tangle-lang-exts '("typescript" . "ts"))

(defun org-babel-variable-assignments:typescript (params)
  "Return PARAMS as list of typescript statements assigning the block's variables."
  (mapcar (lambda (pair) (format "let %s=%s;"
                                 (car pair) (org-babel-typescript-var-to-typescript (cdr pair))))
          (org-babel--get-vars params)))

(defun org-babel-typescript-var-to-typescript (var)
  "Convert an elisp VAR into a string of typescript source code.
specifying a var of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-typescript-var-to-typescript var ", ") "]")
    (replace-regexp-in-string "\n" "\\\\n" (format "%S" var))))

(defun org-babel-execute:typescript (body params)
  "Execute a block of Typescript code BODY and PARAMS with org-babel.
This function is called by `org-babel-execute-src-block'"
  (let* ((tmp-src-file (org-babel-temp-file "ts-src-" ".ts")))
    (with-temp-file tmp-src-file (insert (org-babel-expand-body:generic
                                          body params (org-babel-variable-assignments:typescript params))))
    (let ((results (org-babel-eval (format "ts-node %s 2>&1" (org-babel-process-file-name tmp-src-file)) ""))) results)))


(message "ob-typescript.el")
(provide 'ob-typescript)
;;; ob-typescript.el ends here

