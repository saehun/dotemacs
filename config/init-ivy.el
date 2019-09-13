(when (maybe-require-package 'ivy)
  (add-hook 'after-init-hook 'ivy-mode)
  (setq ivy-height 30)
  (when (maybe-require-package 'counsel)
    (counsel-mode t))
  (maybe-require-package 'smex)
  (after-load 'ivy
    (defun bjm/ivy-yank-whole-word ()
    "Pull next word from buffer into search string."
    (interactive)
    (let (amend)
        (with-ivy-window
        ;;move to last word boundary
        (re-search-backward "\\b")
        (let ((pt (point))
                (le (line-end-position)))
            (forward-word 1)
            (if (> (point) le)
                (goto-char pt)
            (setq amend (buffer-substring-no-properties pt (point))))))
        (when amend
        (insert (replace-regexp-in-string "  +" " " amend)))))
    (define-key ivy-minibuffer-map (kbd "s-f") 'bjm/ivy-yank-whole-word)))

(provide 'init-ivy)
