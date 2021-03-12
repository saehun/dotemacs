;;; init-utils.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))


;;----------------------------------------------------------------------------
;; Handier way to add modes to auto-mode-alist
;;----------------------------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


;;----------------------------------------------------------------------------
;; String utilities missing from core emacs
;;----------------------------------------------------------------------------
(defun sanityinc/string-all-matches (regex str &optional group)
  "Find all matche for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))


;;----------------------------------------------------------------------------
;; Delete the current file
;;----------------------------------------------------------------------------
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))


;;----------------------------------------------------------------------------
;; Rename the current file
;;----------------------------------------------------------------------------
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))


;;----------------------------------------------------------------------------
;; Browse current HTML file
;;----------------------------------------------------------------------------
(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))


;;----------------------------------------------------------------------------
;; open iterm tab with current location
;;----------------------------------------------------------------------------
(defun iterm ()
  "Open the current directory in iterm with new tab."
  (interactive)
  (call-process-shell-command
    "iterm" nil nil nil
    (let ((dir (projectile-root-bottom-up default-directory)))
      (if dir dir default-directory))))


(defun swap-buffers-in-windows ()
  "Put the buffer from the selected window in next window, and vice versa"
  (interactive)
  (let* ((this (selected-window))
     (other (next-window))
     (this-buffer (window-buffer this))
     (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)
    ))


(defun copy-buffers-in-windows ()
  "Put the buffer from the selected window in next window, and vice versa"
  (interactive)
  (let* ((this (selected-window))
     (other (next-window))
     (this-buffer (window-buffer this)))
    (set-window-buffer other this-buffer)
    ))

(defun copy-buffers-in-prev-windows ()
  "Put the buffer from the selected window in next window, and vice versa"
  (interactive)
  (let* ((this (selected-window))
     (other (prev-window))
     (this-buffer (window-buffer this)))
    (set-window-buffer other this-buffer)
    ))

;;----------------------------------------------------------------------------
;; Close buffer
;;----------------------------------------------------------------------------
(defun kill-buffer-and-return-previous ()
  "Kill buffer and return previous buffer"
  (interactive)
  (require 'dash)
  (let (prev) (-copy (previous-buffer))
    (kill-buffer)
    (set-window-buffer prev)))

;;----------------------------------------------------------------------------
;; open iterm tab with current location
;;----------------------------------------------------------------------------
(defun new-empty-buffer ()
  "Open a new empty buffer.
   URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
   Version 2016-08-11"
  (interactive)
  (let ((-buf (generate-new-buffer "untitled")))
    (switch-to-buffer -buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)))


;;----------------------------------------------------------------------------
;; Transparency
;;----------------------------------------------------------------------------
(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 50) '(100 . 100)))))

(defun transparency (value)
   "Set the transparency of the frame window.  0=transparent/100=opaque."
   (interactive "nTransparency Value 0 - 100 opaque:")
   (set-frame-parameter (selected-frame) 'alpha value))


;;----------------------------------------------------------------------------
;; Ivy command history
;;----------------------------------------------------------------------------
(defun counsel-shell-command ()
  "Forward to `shell-command'."
  (interactive)
  (ivy-read "Shell Command: "
                        shell-command-history
            :action (lambda (x)
                      (shell-command x))
            :caller 'counsel-shell-command))


;;----------------------------------------------------------------------------
;; Open ghq project
;;----------------------------------------------------------------------------
(defun counsel-open-project ()
  "Open and select project in cousel buffer."
  (interactive)
  (ivy-read "Shell Command: "
    (split-string (with-output-to-string (call-process "ghq" nil standard-output nil "list")) "\n" t)
    :action (lambda (x) (dired (concat "~/wd/" x)))))


(provide 'init-utils)
;;; init-utils.el ends here
