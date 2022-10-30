;;; package --- node-binding
;;; Commentary:
;;; Code:

(require 'cl-lib)

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

(cl-defmacro post-message-node-thunk (command &optional (data ""))
  (let ((service-env (node-service-env)))
    `(lambda ()
       (with-output-to-string
       (call-process "emacs-node" nil standard-output nil ,command ,data ,service-env)))))

(cl-defun post-message-node-with-env-async (command &optional (data ""))
  "(Async) Send message with env."
  (if (> (buffer-size) 100000) (message "Buffer is to large: %d charaters" (buffer-size))
    (let ((service-env (node-service-env)))
      (async-start
        `(lambda ()
           (with-output-to-string
             (call-process "emacs-node" nil standard-output nil ,command ,data ,service-env)))
        (lambda (response)
          (eval-string response))))))

;; Command
(defun node/counsel-command ()
  "List node command."
  (interactive)
  (post-message-node "list"))

(defun node/counsel-open ()
  "List open."
  (interactive)
  (post-message-node "open-url"))

(defun node/new-test ()
  "Create and open test file."
  (interactive)
  (post-message-node-with-env "ts-unit-test"))

(defun node/wrap-try-catch ()
  "Wrap region with try catch block."
  (interactive)
  (post-message-node-with-env "wrap-try-catch"))

(defun node/run-current-file ()
  "Run current typescript file with ts-node."
  (interactive)
  (post-message-node-with-env-async "run-current-file"))

(defun node/debug-current-file ()
  "Debug current typescript file with ts-node."
  (interactive)
  (post-message-node-with-env-async "debug-current-file"))

(defun node/test-current-file ()
  "Test current typescript file with jest."
  (interactive)
  (post-message-node-with-env-async "test-current-file"))

(defun node/debug-test-current-file ()
  "Test and Debug current typescript file with jest inspect-brk."
  (interactive)
  (post-message-node-with-env-async "debug-test-current-file"))

(defun node/object-to-type ()
  "Convert js object to type literal."
  (interactive)
  (post-message-node-with-env "object-to-type"))

(defun node/object-to-type-with-comment ()
  "Convert js object to type literal."
  (interactive)
  (post-message-node-with-env "object-to-type-with-comment"))

(defun node/object-to-query ()
  "Convert js object to query string."
  (interactive)
  (post-message-node-with-env "object-to-query"))

(defun node/cookie-to-object ()
  "Convert js cookie strings to object."
  (interactive)
  (post-message-node-with-env "cookie-to-object"))

(defun git-open-node-modules ()
  "Open git page with given package name around the cursor."
  (interactive)
  (post-message-node-with-env "git-open-node-modules"))

(defun node/http-to-httpTemplate ()
  "Open git page with given package name around the cursor."
  (interactive)
  (post-message-node-with-env "transform-request" "http-template-requester"))

(require 'find-file-in-project)
(defun node/import ()
  "Import for javascript project."
  (interactive)
  (post-message-node-with-env
    "import-from-project"
    (format "%s" (ffip-project-search "" nil))))

(require 'evil)
(defun node-insert-import-and-complete (path)
  "Insert import statement from PATH."
  (progn
    (evil-set-jump)
    (goto-char 0)
    (insert (concat "import {  } from '" path "';\n"))
    (goto-char 10)
    (evil-insert-state)
    (company-complete)))

(defun typescript-playground ()
  "Open typescript playground with given region."
  (interactive)
  (post-message-node-with-env "typescript-playground"))

(defun jwt-decode ()
  "Decode and replace jwt string in the region."
  (interactive)
  (post-message-node-with-env "jwt-decode"))

(defun tmp-call ()
  "Hmm."
  (interactive)
  (message (with-output-to-string (call-process "env" nil standard-output))))

(defun blame ()
  "Blame line with git and open corresponed PR or commit on github."
  (interactive)
  (post-message-node-with-env "blame" (format "%s" (nth 2 (ghq-parse)))))

(defun blame-commit ()
  "Blame line with git and open corresponed PR or commit on github."
  (interactive)
  (post-message-node-with-env "blame-commit" (format "%s" (nth 2 (ghq-parse)))))

(defun ts-syntax-kind ()
  "Print ts-syntax-kind on current cursor."
  (interactive)
  (post-message-node-with-env "ts-syntax-kind"))

(defun ts-ast-tree ()
  "Print ts ast tree on current cursor."
  (interactive)
  (post-message-node-with-env-async "ts-ast-tree"))

(defun ts-factory-code-gen ()
  "Create factory code."
  (interactive)
  (post-message-node-with-env-async "ts-factory-code-gen" "buffer"))

(defun ts-factory-code-gen-region ()
  "Create factory code."
  (interactive)
  (post-message-node-with-env-async "ts-factory-code-gen" "region"))

(defun ts-transpile-region-and-copy ()
  "Transpile typescript code in region and copy the result."
  (interactive)
  (post-message-node-with-env-async "ts-transpile-region-and-copy"))

(provide 'node-binding)
;;; node-binding.el ends here



