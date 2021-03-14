;;; package --- node-binding
;;; Commentary:
;;; Code:

(require 'cl-lib)

(cl-defun post-message-node (command &optional (data ""))
  "Send message."
  (eval-string
    (with-output-to-string
      (call-process "emacs-node" nil standard-output nil command data))))

(cl-defun post-message-node-with-env (command &optional (data ""))
  "Send message with env."
  (if (> (buffer-size) 100000) (message "Buffer is to large: %d charaters" (buffer-size))
  (eval-string
    (with-output-to-string
      (call-process "emacs-node" nil standard-output nil command data (node-service-env))))))

(defun eval-string (string)
  "Evaluate elisp code stored in a string."
  (eval (car (read-from-string string))))

(defun node-service-env ()
  "Execute `emacs-node' nodejs command and evaluate it's standard output."
  (let ((directory default-directory)
         (filename (buffer-name))
         (buffer (buffer-string))
         (region (if (region-active-p) ;; region
                    (filter-buffer-substring (region-beginning) (region-end))
                    "")))
    (json-encode
      (list
        (cons "directory" directory)
        (cons "cursor" (list
          (cons "pos" (point))
          (cons "col" (current-column))
          (cons "row" (line-number-at-pos))))
        (cons "filename" filename)
        (cons "buffer" buffer)
        (cons "region" region)))))

;; Command
(defun node/counsel-command ()
  "List node command."
  (interactive)
  (post-message-node-with-env "list"))

(defun node/counsel-open ()
  "List open."
  (interactive)
  (post-message-node "open"))

(provide 'node-binding)
;;; node-binding.el ends here
