;;; init-ts-hydra.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; 
;;;; Requirements:
(require 'hydra)
(require 'init-utils)
(require 'node-binding)
(require 'tide)

;;; Code:
(defun typescript-in-testfile-p ()
  "Tell whether current buffer is testfile or not."
  (or (cl-search "test.ts" (file-name-nondirectory (buffer-file-name)))
      (cl-search "spec.ts" (file-name-nondirectory (buffer-file-name)))))

(defun node/open-nearest-pakcage-json ()
  "Open nearest package.json."
  (interactive)
  (find-file (concat (projectile-locate-dominating-file default-directory "package.json") "/package.json")))


;;; Hydra:
(defhydra hydra-typescript (:color pink
                                   :hint nil
                                   :exit t)
  "

  ^Command^                   ^Test^                       ^Goto^
  ^^^^^^^^----------------------------------------------------------------------------
  _r_: run                  _t_: create or goto test.ts    _p_: nearest package.json
  _v_: run (vite)           _f_: apply all code fix       
  _d_: debug                                             
  _w_: watch
  _o_: organize import
  _e_: list buffer errors
  _E_: list project errors

"
  ("r" node/run-current-file)
  ("v" node/run-current-file/vite-node)
  ("d" node/debug-current-file)
  ("w" node/watch-run-current-file)
  ("t" node/new-test)
  ("f" tide-fix-all)
  ("o" tide-organize-imports)
  ("e" flycheck-list-errors)
  ("E" tide-project-errors)
  ("p" node/open-nearest-pakcage-json)

  ("C-e" nil "quit")
  ("q" quit-window "quit" :color blue))

(defhydra hydra-typescript-test (:color pink
                                        :hint nil
                                        :exit t)
  "

  ^Command^         ^Move^
  ^^^^^^^^---------------------------------
  _t_: test       _s_: goto back to source
  _v_: vitest
  _d_: debug
  _w_: watch

"
  ("t" node/test-current-file)
  ("v" node/vitest-current-file)
  ("d" node/debug-test-current-file)
  ("w" node/watch-test-current-file)
  ("s" node/new-test)

  ("s-e" nil "quit")
  ("e" tide-project-errors "list errors" :color blue)
  ("q" quit-window "quit" :color blue))

(define-key tide-mode-map (kbd "C-e")
            (lambda ()
              (interactive)
              (if (typescript-in-testfile-p)
                  (hydra-typescript-test/body)
                (hydra-typescript/body))))

(message "init-ts-hydra.el")
(provide 'init-ts-hydra)
;;; init-ts-hydra.el ends here

