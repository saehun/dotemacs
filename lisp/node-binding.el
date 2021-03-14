;;; package --- node-binding
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'request)

(defun websocket-client-with-remote-server ()
  ;; Emacs previous to Emacs 24 cannot handle wss.
  ;; echo.websocket.org has an untrusted certificate, for the test to
  ;; proceed, we need to disable trust checking.
  (interactive)
  (request
    "http://localhost:10684"
    :type "POST"
    :data (json-encode '(("key" . "value") ("key2" . "value2")))
    :headers '(("Content-Type" . "application/json"))
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (message data)))))

(defun eval-string (string)
  "Evaluate elisp code stored in a string."
  (eval (car (read-from-string string))))

(defun emacs-node-exec (command)
  "Execute `emacs-node' nodejs command and evaluate it's standard output."
  (if (> (buffer-size) 100000) (message "Buffer is to large: %d charaters" (buffer-size))
  (let (
         (directory default-directory)
         (filename (buffer-name))
         (cursor (format
                   "{ \"pos\": %d, \"col\": %d, \"row\": %d }"
                   (point)
                   (current-column)
                   (line-number-at-pos)))
         (buffer (buffer-string))
         (region (if (region-active-p) ;; region
                    (filter-buffer-substring (region-beginning) (region-end))
                    ""))
         )
  (eval-string
    (with-output-to-string
      (call-process "emacs-node" nil standard-output nil
        command
        directory
        filename
        cursor
        buffer
        region
        ))
    ))))

(defun emacs-node ()
  "Json to Yaml."
  (interactive)
  (emacs-node-exec "hi"))

(provide 'node-binding)
;;; node-binding.el ends here
