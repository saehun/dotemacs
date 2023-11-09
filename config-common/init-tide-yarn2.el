;;; ramblehead's yarn 2 virtual paths support for tide

(require 'tide)
(require 'arc-mode)

(defun tide--get-arc-path-pair (full-path)
  ;; \\|\\\\ in case of some substandard OSes ;)
  ;; Is there some safer/more-generic way to split/join paths in elisp?
  ;; Possibly, OS-neutral and with URL/protocol prefixes?
  (let ((path-components (split-string full-path "/\\|\\\\"))
        (arc-path "")
        (file-path-in-arc "")
        arc-found)
    ;; Distinguishing absolute and relative paths - i.e. trailing "/".
    (unless (string-empty-p (car path-components))
      (setq arc-path (car path-components)))
    (setq path-components (cdr path-components))
    (seq-do
     (lambda (component)
       (if arc-found
           (setq file-path-in-arc (concat file-path-in-arc "/" component))
         (setq arc-path (concat arc-path "/" component))
         (when (and (file-regular-p arc-path)
                    (with-temp-buffer
                      ;; 300000 is a magic number - it should
                      ;; be more than enough to recognise any achieve
                      ;; type header.
                      (insert-file-contents arc-path nil 0 300000)
                      (ignore-errors (archive-find-type))))
           (setq arc-found t))))
     path-components)
    (and arc-found
         (not (string-empty-p arc-path))
         (not (string-empty-p file-path-in-arc))
         (cons arc-path (substring file-path-in-arc 1)))))

(defun tide--yarn2-resolve-virtual (path)
  ;; See https://yarnpkg.com/advanced/pnpapi#resolvevirtual
  ;; and https://github.com/yarnpkg/berry/issues/499#issuecomment-539458981
  (save-match-data
    (let ((sep "/")
          tail hash depth head)
      (if (not (string-match
                "\\(.*\\)/\\$\\$virtual/\\([^/]+\\)/\\([0-9]+\\)/\\(.*\\)"
                path))
          path
        ;; Strings
        (setq tail (match-string 1 path))
        (setq hash (match-string 2 path))
        (setq depth (match-string 3 path))
        (setq head (match-string 4 path))
        ;; Data
        (set-text-properties 0 (length sep) (text-properties-at 0 path) sep)
        (setq depth (string-to-number depth))
        (setq tail (split-string tail "/"))
        (setq tail (seq-subseq tail 0 (when (> depth 0) (- depth))))
        (setq head (split-string head "/"))
        (string-join (append tail head) sep)))))

(defun tide-get-file-buffer:around (oldfun file &optional new-file)
  "Returns a buffer associated with a file. This will return the
current buffer if it matches FILE. Then it will try to resolve
yarn 2 virtual path in archives and unplugged. Then it will call
the original tide-get-file-buffer() function as oldfun()."
  (let ((file-virtual-resolved (tide--yarn2-resolve-virtual file))
        arc-path-pair)
    (cond
     ((equal file (tide-buffer-file-name)) (current-buffer))
     ((setq arc-path-pair (tide--get-arc-path-pair file-virtual-resolved))
      (let ((arc-path (car arc-path-pair))
            (file-path-in-arc (cdr arc-path-pair))
            arc-buf)
        (setq arc-buf (find-file-noselect arc-path))
        (with-current-buffer arc-buf
          (goto-char (point-min))
          ;; This should fail in nested archives.
          (re-search-forward (concat " " file-path-in-arc "$"))
          (archive-extract))))
     ((file-exists-p file-virtual-resolved)
      (find-file-noselect file-virtual-resolved))
     (t (funcall oldfun file new-file)))))

(defun tide-eldoc-maybe-show:around (oldfun text)
  "Tests if TEXT has any \"string\" with yarn 2 virtual path. If
there is such \"string\", then resolve it to conventional path and
override \"string\" in TEXT with resolved conventional path.
Then call the original tide-eldoc-maybe-show() function as oldfun()."
  (save-match-data
    (let (tail head virtual-path)
      (string-match
       "\\(.*\\\"\\)\\(.*/\\$\\$virtual/[^/]+/[0-9]+/.*\\\"\\)\\(.*\\)"
       text)
      (setq virtual-path (match-string 2 text))
      (when virtual-path
        (setq text
              (concat
               (match-string 1 text)
               (tide--yarn2-resolve-virtual virtual-path)
               (match-string 3 text))))))
  (funcall oldfun text))

(defun tide-yarn2-enable ()
  (interactive)
  (advice-add 'tide-get-file-buffer :around
              #'tide-get-file-buffer:around)
  (advice-add 'tide-eldoc-maybe-show :around
              #'tide-eldoc-maybe-show:around)
  (message "Yarn 2 support enabled"))

(defun tide-yarn2-disable ()
  (interactive)
  (advice-remove 'tide-get-file-buffer
                 #'tide-get-file-buffer:around)
  (advice-remove 'tide-eldoc-maybe-show
                 #'tide-eldoc-maybe-show:around)
  (message "Yarn 2 support disabled"))

(provide 'init-tide-yarn2)
