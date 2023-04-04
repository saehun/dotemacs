;;; package --- init-hydra-typescript
;;; Commentary:
;;; Code:

(require 'hydra)
(require 'init-utils)
(require 'web-mode)
(require 'node-binding)

(defhydra hydra-typescript (:color pink
                             :hint nil
                             :exit t)
  "

  ^Command^         ^Test^
  ^^^^^^^^--------------------------
  _r_: run        _t_: create or goto test.ts
  _d_: debug      _f_: apply all code fix
  _w_: watch

"
  ("r" node/run-current-file)
  ("d" node/debug-current-file)
  ("w" node/watch-run-current-file)
  ("t" node/new-test)
  ("f" tide-fix-all)

  ("s-e" nil "quit")
  ("e" flycheck-list-errors "list errors" :color blue)
  ("q" quit-window "quit" :color blue))

(defhydra hydra-typescript-test (:color pink
                             :hint nil
                             :exit t)
  "

  ^Command^         ^Move^
  ^^^^^^^^--------------------------
  _t_: test       _s_: goto back to source
  _d_: debug
  _w_: watch

"
  ("t" node/test-current-file)
  ("d" node/debug-test-current-file)
  ("w" node/watch-test-current-file)
  ("s" node/new-test)

  ("s-e" nil "quit")
  ("e" flycheck-list-errors "list errors" :color blue)
  ("q" quit-window "quit" :color blue))

(defun typescript-in-testfile-p ()
  "Tell whether current buffer is testfile or not."
  (cl-search "test.ts" (file-name-nondirectory (buffer-file-name))))

(define-key web-mode-map (kbd "s-e")
  (lambda ()
    (interactive)
    (if (typescript-in-testfile-p)
      (hydra-typescript-test/body)
      (hydra-typescript/body))))

(provide 'init-hydra-typescript)

;;; init-hydra-typescript.el ends here
