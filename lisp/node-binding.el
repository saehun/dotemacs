;;; package --- node-binding
;;; Commentary:
;;; Code:

(require 'cl-lib)

(defun choose-directory (directory-to-start-in)
  "Return a directory chosen by the user.  The user will be prompted
to choose a directory starting with `directory-to-start-in'"
  (let* ((ivy-read-prompt "Directory: ")
         (counsel--find-file-predicate #'file-directory-p)
         (default-directory directory-to-start-in)
         (selected-directory
          (ivy-read
           ivy-read-prompt
           #'read-file-name-internal
           :matcher #'counsel--find-file-matcher)))
    selected-directory))

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
         (filename (if (buffer-file-name)
                     (file-name-nondirectory (buffer-file-name))
                     ""))
         (buffer (buffer-string))
         (region (if (region-active-p) ;; region
                    (filter-buffer-substring (region-beginning) (region-end))
                    "")))
    (json-encode
      (list
        (cons "mode" (format "%s" major-mode))
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
  (post-message-node "list"))

(defun node/counsel-open ()
  "List open."
  (interactive)
  (post-message-node "open-url"))

(defun node/ts-unit-test ()
  "Create and open test file."
  (interactive)
  (post-message-node-with-env "ts-unit-test"))

(provide 'node-binding)
;;; node-binding.el ends here
