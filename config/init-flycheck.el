;;; init-flycheck.el --- Configure Flycheck global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(when (maybe-require-package 'flycheck)
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-global-modes '(not js-mode javascript-mode))

  ;; https://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  (defun my/use-eslint-from-node-modules ()
    (let* ((eslint (find-executable-in-node_modules-monorepo "eslint")))
      (when eslint
        (setq-local flycheck-javascript-eslint-executable eslint))))

  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))

  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (setq flycheck-emacs-lisp-load-path
    (list
      (expand-file-name "config" user-emacs-directory)
      (expand-file-name "prisma" user-emacs-directory)
      (expand-file-name "lisp" user-emacs-directory)))

  (when (maybe-require-package 'flycheck-color-mode-line)
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))



  ;;----------------------------------------------------------------------------
  ;; Optional: ensure flycheck cycles, both when going backward and forward.
  ;; Tries to handle arguments correctly.
  ;; Since flycheck-previous-error is written in terms of flycheck-next-error,
  ;; advising the latter is enough.
  (defun flycheck-next-error-loop-advice (orig-fun &optional n reset)
    (condition-case err
      (apply orig-fun (list n reset))
      ((user-error)
        (let ((error-count (length flycheck-current-errors)))
          (if (and
                (> error-count 0)                   ; There are errors so we can cycle.
                (equal (error-message-string err) "No more Flycheck errors"))
            ;; We need to cycle.
            (let* ((req-n (if (numberp n) n 1)) ; Requested displacement.
                                        ; An universal argument is taken as reset, so shouldn't fail.
                    (curr-pos (if (> req-n 0) (- error-count 1) 0)) ; 0-indexed.
                    (next-pos (mod (+ curr-pos req-n) error-count))) ; next-pos must be 1-indexed
              (apply orig-fun (list (+ 1 next-pos) 'reset)))
            (signal (car err) (cdr err)))))))

  (advice-add 'flycheck-next-error :around #'flycheck-next-error-loop-advice))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
