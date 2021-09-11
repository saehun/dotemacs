(when (maybe-require-package 'ivy)
  (add-hook 'after-init-hook 'ivy-mode)
  (setq ivy-height 20)

  (when (maybe-require-package 'counsel)
    ;;---------------------------
    ;; set google search
    ;;---------------------------
    (when (maybe-require-package 'request)
      (setq counsel-search-engine 'google))
    (counsel-mode t))

  (maybe-require-package 'smex)

  ;; live preview for imenu
  ;; https://github.com/abo-abo/swiper/issues/2188
  (ivy-configure 'counsel-imenu
    :update-fn 'auto)

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
