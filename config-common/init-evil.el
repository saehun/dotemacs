
(setq evil-want-C-u-scroll t)
(require 'evil)
(require 'evil-surround)
(require 'evil-textobj-anyblock)
(require 'evil-textobj-tree-sitter)

(evil-mode 1)
(global-evil-surround-mode 1)

(evil-define-operator wrap-with-parens (beg end)
  (goto-char beg)
  (insert "(")
  (goto-char (1+ end))
  (insert ")"))

(evil-define-operator wrap-with-quote (beg end)
  (goto-char beg)
  (insert "'")
  (goto-char (1+ end))
  (insert "'")
  (goto-char end))

(evil-define-operator wrap-with-brace (beg end)
  (goto-char beg)
  (insert "{")
  (goto-char (1+ end))
  (insert "}")
  (goto-char end))

(evil-define-operator wrap-with-bracket (beg end)
  (goto-char beg)
  (insert "[")
  (goto-char (1+ end))
  (insert "]")
  (goto-char end))

(evil-define-operator wrap-with-backtick (beg end)
  (goto-char beg)
  (insert "`")
  (goto-char (1+ end))
  (insert "`")
  (goto-char end))

(evil-define-key 'visual global-map (kbd ")") 'wrap-with-parens)
(evil-define-key 'visual global-map (kbd "'") 'wrap-with-quote)
(evil-define-key 'visual global-map (kbd "}") 'wrap-with-brace)
(evil-define-key 'visual global-map (kbd "]") 'wrap-with-bracket)
(evil-define-key 'visual global-map (kbd "`") 'wrap-with-backtick)


;; http://blog.binchen.org/posts/enhance-emacs-evil-global-markers.html
(require 'evil)
(defvar evil-global-markers-history nil)
(setq evil-kill-on-visual-paste nil)
(setq evil-shift-width 2)
(defun my-forward-line (lnum)
  "Forward LNUM lines."
  (setq lnum (string-to-number lnum))
  (when (and lnum (> lnum 0))
    (goto-char (point-min))
    (forward-line (1- lnum))))

(defadvice evil-set-marker (before evil-set-marker-before-hack activate)
  (let* ((args (ad-get-args 0))
         (c (nth 0 args))
         (pos (or (nth 1 args) (point))))
    ;; only rememeber global markers
    (when (and (>= c ?A) (<= c ?Z) buffer-file-name)
      (setq evil-global-markers-history
            (delq nil
                  (mapcar `(lambda (e)
                             (unless (string-match (format "^%s@" (char-to-string ,c)) e)
                               e))
                          evil-global-markers-history)))
      (setq evil-global-markers-history
            (add-to-list 'evil-global-markers-history
                         (format "%s@%s:%d:%s"
                                 (char-to-string c)
                                 (file-truename buffer-file-name)
                                 (line-number-at-pos pos)
                                 (string-trim (buffer-substring-no-properties (line-beginning-position)
                                                                              (line-end-position)))))))))

(defadvice evil-goto-mark-line (around evil-goto-mark-line-hack activate)
  (let* ((args (ad-get-args 0))
         (c (nth 0 args))
         (orig-pos (point)))

    (condition-case nil
        ad-do-it
      (error (progn
               (when (and (eq orig-pos (point)) evil-global-markers-history)
                 (let* ((markers evil-global-markers-history)
                        (i 0)
                        m
                        file
                        found)
                   (while (and (not found) (< i (length markers)))
                     (setq m (nth i markers))
                     (when (string-match (format "\\`%s@\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'"
                                                 (char-to-string c))
                                         m)
                       (setq file (match-string-no-properties 1 m))
                       (setq found (match-string-no-properties 2 m)))
                     (setq i (1+ i)))
                   (when file
                     (find-file file)
                     (my-forward-line found)))))))))

(defun counsel-evil-goto-global-marker ()
  "Goto global evil marker."
  (interactive)
  (unless (featurep 'ivy) (require 'ivy))
  (ivy-read "Goto global evil marker"
            evil-global-markers-history
            :action (lambda (m)
                      (when (string-match "\\`[A-Z]@\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" m)
                        (let* ((file (match-string-no-properties 1 m))
                               (linenum (match-string-no-properties 2 m)))
                          (find-file file)
                          (my-forward-line linenum))))))


;; disable evil mouse motion for performance issue
;; https://stackoverflow.com/questions/46513910/emacs-evil-mode-binding-mouse-event
(with-eval-after-load 'evil-maps (define-key evil-motion-state-map [down-mouse-1] nil))
(with-eval-after-load 'evil-maps (define-key evil-motion-state-map (kbd "RET") nil))

;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
(define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
(define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
;; You can also bind multiple items and we will match the first one we can find
(define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))

;; Goto start of next function
(define-key evil-normal-state-map
            (kbd "]f")
            (lambda ()
              (interactive)
              (evil-textobj-tree-sitter-goto-textobj "function.outer")))

;; Goto start of previous function
(define-key evil-normal-state-map
            (kbd "[f")
            (lambda ()
              (interactive)
              (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))

;; Goto end of next function
(define-key evil-normal-state-map
            (kbd "]F")
            (lambda ()
              (interactive)
              (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)))

;; Goto end of previous function
(define-key evil-normal-state-map
            (kbd "[F")
            (lambda ()
              (interactive)
              (evil-textobj-tree-sitter-goto-textobj "function.outer" t t)))

(provide 'init-evil)
