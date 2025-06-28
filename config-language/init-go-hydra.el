;;; init-go-hydra.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Hydra configuration for Go development
;;;; Requirements:
(require 'hydra)
(require 'go-ts-mode)
(require 'lsp-mode)
(require 'projectile)

;;; Code:
(defun execute-go-command-in-iterm (command &optional args)
  "Execute go COMMAND with optional ARGS in iTerm using find-session."
  (let* ((project-root (or (projectile-project-root) default-directory))
         (cleaned-root (replace-regexp-in-string "/$" "" project-root))
         (full-command (if args
                          (format "%s %s" command args)
                        command)))
    (call-process-shell-command
     (format "find-session %s \"%s\"" cleaned-root full-command) nil nil nil)))
(defun go-in-testfile-p ()
  "Tell whether current buffer is a Go test file or not."
  (string-match-p "_test\\.go$" (or (buffer-file-name) "")))

(defun go-run-current-package ()
  "Run the current Go package in iTerm."
  (interactive)
  (execute-go-command-in-iterm "go run ."))

(defun go-build-current-package ()
  "Build the current Go package in iTerm."
  (interactive)
  (execute-go-command-in-iterm "go build ."))

(defun go-test-current-function ()
  "Run the test function at point in iTerm."
  (interactive)
  (let ((test-name (go-test--get-current-test)))
    (if test-name
        (execute-go-command-in-iterm "go test -v -run" test-name)
      (message "No test function found at point"))))

(defun go-test--get-current-test ()
  "Get the current test function name."
  (save-excursion
    (re-search-backward "^func[ ]+\\(Test[a-zA-Z0-9_]*\\)" nil t)
    (match-string 1)))

(defun go-benchmark-current-function ()
  "Run the benchmark function at point in iTerm."
  (interactive)
  (let ((bench-name (go-benchmark--get-current)))
    (if bench-name
        (execute-go-command-in-iterm (format "go test -bench=%s -run=^$" bench-name))
      (message "No benchmark function found at point"))))

(defun go-benchmark--get-current ()
  "Get the current benchmark function name."
  (save-excursion
    (re-search-backward "^func[ ]+\\(Benchmark[a-zA-Z0-9_]*\\)" nil t)
    (match-string 1)))

(defun go-toggle-test-file ()
  "Toggle between implementation and test file."
  (interactive)
  (let* ((file-name (file-name-nondirectory (buffer-file-name)))
         (dir-name (file-name-directory (buffer-file-name)))
         (is-test (string-match-p "_test\\.go$" file-name)))
    (if is-test
        (let ((impl-file (replace-regexp-in-string "_test\\.go$" ".go" file-name)))
          (find-file (concat dir-name impl-file)))
      (let ((test-file (replace-regexp-in-string "\\.go$" "_test.go" file-name)))
        (find-file (concat dir-name test-file))))))

(defun go-open-go-mod ()
  "Open the go.mod file in the project root."
  (interactive)
  (let ((go-mod (locate-dominating-file default-directory "go.mod")))
    (if go-mod
        (find-file (concat go-mod "go.mod"))
      (message "No go.mod file found"))))

(defun go-run-tests-regexp-iterm (regexp)
  "Run Go tests matching REGEXP in iTerm."
  (interactive "sTest regexp: ")
  (execute-go-command-in-iterm (format "go test -v -run %s" regexp)))

(defun go-get-package-iterm (package)
  "Run go get PACKAGE in iTerm."
  (interactive "sPackage name: ")
  (execute-go-command-in-iterm (format "go get %s" package)))

;;; Hydra for regular Go files:
(defhydra hydra-go (:color pink
                    :hint nil
                    :exit t)
  "
  ^Build/Run^             ^Test^                    ^Code^                   ^Navigate^
  ^^^^^^^^-----------------------------------------------------------------------------------------
  _r_: run package      _t_: test function        _o_: organize imports    _m_: go.mod
  _b_: build package    _f_: test file            _a_: code action         _T_: toggle test
  _B_: benchmark        _p_: test package         _i_: impl interface      _d_: definition
  _R_: run regexp       _c_: coverage             _s_: fill struct         _D_: describe
  _e_: errors           _v_: verbose test         _g_: generate            _u_: references
                                                _G_: go get              
  ^Module^              ^Refactor^                ^Tag^
  ^^^^^^^^-----------------------------------------------------------------------------------------
  _mt_: mod tidy        _rr_: rename              _ta_: add tag
  _md_: mod download    _re_: extract             _tr_: remove tag
  _mi_: mod init        _rt_: toggle
"
  ;; Build/Run
  ("r" go-run-current-package)
  ("b" go-build-current-package)
  ("B" go-benchmark-current-function)
  ("R" (call-interactively 'go-run-tests-regexp-iterm))
  ("e" flycheck-list-errors)
  
  ;; Test
  ("t" go-test-current-function)
  ("f" (execute-go-command-in-iterm "go test -v ."))
  ("p" (execute-go-command-in-iterm "go test -v ./..."))
  ("c" (execute-go-command-in-iterm "go test -coverprofile=coverage.out && go tool cover -html=coverage.out"))
  ("v" (execute-go-command-in-iterm "go test -v"))
  
  ;; Code
  ("o" lsp-organize-imports)
  ("a" lsp-execute-code-action)
  ("i" go-impl)
  ("s" go-fill-struct)
  ("g" go-gen-test-dwim)
  ("G" (call-interactively 'go-get-package-iterm))
  
  ;; Navigate
  ("m" go-open-go-mod)
  ("T" go-toggle-test-file)
  ("d" lsp-find-definition)
  ("D" lsp-describe-thing-at-point)
  ("u" lsp-find-references)
  
  ;; Module
  ("mt" (execute-go-command-in-iterm "go mod tidy"))
  ("md" (execute-go-command-in-iterm "go mod download"))
  ("mi" (execute-go-command-in-iterm "go mod init"))
  
  ;; Refactor
  ("rr" lsp-rename)
  ("re" godoctor-extract)
  ("rt" godoctor-toggle)
  
  ;; Tag
  ("ta" go-tag-add)
  ("tr" go-tag-remove)
  
  ("C-e" nil "quit")
  ("q" quit-window "quit" :color blue))

;;; Hydra for Go test files:
(defhydra hydra-go-test (:color pink
                         :hint nil
                         :exit t)
  "
  ^Test Commands^         ^Navigate^               ^Benchmark^
  ^^^^^^^^---------------------------------------------------------------
  _t_: test function    _s_: goto source         _b_: bench function
  _f_: test file        _T_: toggle test         _B_: bench all
  _p_: test package     _m_: go.mod              
  _d_: debug test       
  _c_: coverage         
  _v_: verbose test     
  _r_: run regexp       
  
  ^Code^                 
  ^^^^^^^^---------------------------------------------------------------
  _o_: organize imports    
  _a_: code action      
  _g_: generate test    
"
  ;; Test Commands
  ("t" go-test-current-function)
  ("f" (execute-go-command-in-iterm "go test -v ."))
  ("p" (execute-go-command-in-iterm "go test -v ./..."))
  ("d" (let ((test-name (go-test--get-current-test)))
         (if test-name
             (execute-go-command-in-iterm (format "dlv test -- -test.run %s" test-name))
           (message "No test function found at point"))))
  ("c" (execute-go-command-in-iterm "go test -coverprofile=coverage.out && go tool cover -html=coverage.out"))
  ("v" (execute-go-command-in-iterm "go test -v"))
  ("r" (call-interactively 'go-run-tests-regexp-iterm))
  
  ;; Navigate
  ("s" go-toggle-test-file)
  ("T" go-toggle-test-file)
  ("m" go-open-go-mod)
  
  ;; Benchmark
  ("b" go-benchmark-current-function)
  ("B" (execute-go-command-in-iterm "go test -bench=."))
  
  ;; Code
  ("o" lsp-organize-imports)
  ("a" lsp-execute-code-action)
  ("g" go-gen-test-dwim)
  
  ("C-e" nil "quit")
  ("e" flycheck-list-errors "list errors" :color blue)
  ("q" quit-window "quit" :color blue))

;; Key binding
(with-eval-after-load 'go-ts-mode
  (define-key go-ts-mode-map (kbd "C-e")
              (lambda ()
                (interactive)
                (if (go-in-testfile-p)
                    (hydra-go-test/body)
                  (hydra-go/body)))))

(message "init-go-hydra.el loaded")
(provide 'init-go-hydra)
;;; init-go-hydra.el ends here