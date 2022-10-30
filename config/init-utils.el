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
  (let*
    ((location
       (or (projectile-locate-dominating-file default-directory "package.json")
         (or (projectile-locate-dominating-file default-directory ".git") default-directory)))
      (target-shell-command (format "open -a iTerm %s" location)))
  (progn
    (message target-shell-command)
    (call-process-shell-command target-shell-command nil nil nil))))

;;----------------------------------------------------------------------------
;; open iterm tab with current location
;;----------------------------------------------------------------------------
(defun iterm-here ()
  "Open the current directory in iterm with new tab."
  (interactive)
  (let*
    ((location default-directory)
      (target-shell-command (format "open -a iTerm %s" location)))
  (progn
    (message target-shell-command)
    (call-process-shell-command target-shell-command nil nil nil))))


(defun swap-buffers-in-windows ()
  "Put the buffer from the selected window in next window, and vice versa."
  (interactive)
  (let* ((this (selected-window))
     (other (next-window))
     (this-buffer (window-buffer this))
     (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)
    ))


(defun copy-buffers-in-windows ()
  "Put the buffer from the selected window in next window, and vice versa."
  (interactive)
  (let* ((this (selected-window))
     (other (next-window))
     (this-buffer (window-buffer this)))
    (set-window-buffer other this-buffer)
    ))

(defun copy-buffers-in-prev-windows ()
  "Put the buffer from the selected window in next window, and vice versa."
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
  "Kill buffer and return previous buffer."
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
URL `http://xahlee.info/emacs/emacs/emacs_new_empty_buffer.html'
Version 2016-08-11"
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (setq initial-major-mode 'text-mode)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)))


;;----------------------------------------------------------------------------
;; Transparency
;;----------------------------------------------------------------------------
(defun toggle-transparency ()
  "Toggle transparency."
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
  (ivy-read "Repository: "
    (split-string (with-output-to-string (call-process "ghq-list-append" nil standard-output nil)) "\n" t)
    :action (lambda (x) (dired (concat "~/repo/" x)))))

;;----------------------------------------------------------------------------
;; Open current codebase in VSCode
;;----------------------------------------------------------------------------
(defun code ()
  "Open and select project in cousel buffer."
  (interactive)
  ((lambda (dir) (call-process-shell-command "code" nil nil nil dir))
  (or (projectile-locate-dominating-file default-directory "package.json")
    (or (projectile-root-bottom-up default-directory)
      default-directory))))

;;----------------------------------------------------------------------------
;; Open current codebase in Webstorm
;;----------------------------------------------------------------------------
(defun webstorm ()
  "Open and select project in cousel buffer."
  (interactive)
  ((lambda (dir) (call-process-shell-command "webstorm" nil nil nil dir))
  (or (projectile-locate-dominating-file default-directory "package.json")
    (or (projectile-root-bottom-up default-directory)
      default-directory))))

;;----------------------------------------------------------------------------
;; Open current codebase in fork(git gui)
;;----------------------------------------------------------------------------
(defun fork ()
  "Open and select project in cousel buffer."
  (interactive)
  ((lambda (dir) (call-process-shell-command "fork" nil nil nil dir))
  (or (projectile-locate-dominating-file default-directory ".git")
    (or (projectile-root-bottom-up default-directory)
      default-directory))))

;;----------------------------------------------------------------------------
;; Grep from package root (not a git root)
;;----------------------------------------------------------------------------
(defun counsel-rg-package ()
    "Like `counsel-rg' but always search from the package root, not git root."
    (interactive)
    (counsel-rg nil (projectile-root-bottom-up default-directory '("package.json"))))


(defun ghq-parse ()
  (let*
    ((ghq-root (expand-file-name "~/repo"))
      (is-inside-ghq (s-prefix? ghq-root (expand-file-name default-directory)))
      (is-directory (not (buffer-file-name))))
    (if (not is-inside-ghq)
      (error "You are not inside ghq-tree")
      (let*
        ((full-path (expand-file-name (if is-directory default-directory (buffer-file-name))))
          (seq-path (nthcdr 3 (seq-filter (lambda (x) (not (seq-empty-p x))) (split-string full-path "/"))))
          (git-url (string-join (seq-take seq-path 3) "/")))
        (list full-path seq-path git-url is-directory)))))

(defun pr ()
  "Open pull request page for given repository."
  (interactive)
  (cl-destructuring-bind
    (_ _ git-url _)
    (ghq-parse)
    (browse-url (concat "https://" git-url "/pulls"))))



;;----------------------------------------------------------------------------
;; ghq-util
;;----------------------------------------------------------------------------
(defun ghq-util (action revision)
  (cl-destructuring-bind
    (_ seq-path git-url is-directory)
    (ghq-parse)
    (let* ((git-file-path (string-join (nthcdr 3 seq-path) "/"))
            (line-number
              (if is-directory
                ""
                (if (region-active-p)
                  (format "#L%dL%d"
                    (save-excursion
                      (goto-char (region-beginning))
                      (line-number-at-pos))
                    (save-excursion
                      (goto-char (region-end))
                      (- (line-number-at-pos) 1)))
                  (format "#L%d" (line-number-at-pos)))))
            (url (concat "https://" git-url "/tree/" revision "/" git-file-path line-number)))
      (funcall action url)
      (message url))))

(defun ghq-open ()
  "Open current line in browser."
  (interactive)
  (ghq-util
    'browse-url
    (string-trim (shell-command-to-string "git rev-parse HEAD"))))

(defun ghq-copy ()
  "Copy git url of current line."
  (interactive)
  (ghq-util
    'kill-new
    (string-trim (shell-command-to-string "git rev-parse HEAD"))))

(defun ghq-open-master ()
  "Open current line in browser."
  (interactive)
  (ghq-util
    'browse-url
    "master"))

(defun ghq-copy-master ()
  "Copy git url of current line."
  (interactive)
  (ghq-util
    'kill-new
    "master"))

;;----------------------------------------------------------------------------
;; Switch to Last Buffer
;;----------------------------------------------------------------------------
(defun switch-to-last-buffer ()
  "Switch to last buffer which is buried."
  (interactive)
  (switch-to-buffer (last-buffer)))

;;----------------------------------------------------------------------------
;; Kill all buffers
;;----------------------------------------------------------------------------
(defun kill-all-buffers ()
  "Kill all buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;;----------------------------------------------------------------------------
;; Manually call GC
;;----------------------------------------------------------------------------
(defun call-gc ()
  "Manually call gc."
  (interactive)
  (garbage-collect))

;;----------------------------------------------------------------------------
;; Git open
;;----------------------------------------------------------------------------
(defun git-open ()
  "Open github."
  (interactive)
  (call-process "git-open" nil standard-output nil))

;;----------------------------------------------------------------------------
;; Other window trreemacs 
;;----------------------------------------------------------------------------
(defun other-window-treemacs ()
  "Goto other window and open treemacs."
  (interactive)
  (progn
    (other-window 1)
    (if (eq major-mode 'treemacs-mode) (other-window 1))))


;;----------------------------------------------------------------------------
;; Safe-invoke-prettier
;;----------------------------------------------------------------------------
(defun safe-invoke-prettier ()
  "Enable prettier-js-mode if an rc file is located."
  (interactive)
  (require 'prettier-js)
  (if (or
        (locate-dominating-file default-directory ".prettierrc.json")
        (locate-dominating-file default-directory ".prettierrc.js")
        (locate-dominating-file default-directory ".prettierrc"))
    (progn (prettier-js) (save-buffer))
    (save-buffer)))

;;----------------------------------------------------------------------------
;; Save-And-invoke-prettier
;;----------------------------------------------------------------------------
(defun save-and-invoke-prettier ()
  "Enable prettier-js-mode if an rc file is located."
  (interactive)
  (require 'prettier-js)
  (progn (prettier-js) (save-buffer)))


;;----------------------------------------------------------------------------
;; tide-hl-identifier-and-eldoc
;;----------------------------------------------------------------------------
(defun tide-hl-identifier-and-eldoc ()
  "Enable prettier-js-mode if an rc file is located."
  (interactive)
  (require 'eldoc)
  (require 'tide)
  (progn
    (tide-hl-identifier)
    (eldoc)))

;;----------------------------------------------------------------------------
;; copy the current path
;;----------------------------------------------------------------------------
(defun path-copy ()
  "Copy the current directory path."
  (interactive)
  (kill-new  default-directory)
  (message (concat "(copied) " default-directory)))

;;----------------------------------------------------------------------------
;; Node insert import
;;----------------------------------------------------------------------------
(defun node-insert-import-if-not-found (symbol package-name)
  "Add import statement at the top of the js/ts buffer, if given symbol not exists."
  (if (save-excursion
        (save-match-data
          (goto-char (point-min))
          (re-search-forward
            (format "{[\na-zA-Z0-9,[:space:]]+%s[\na-zA-Z0-9,[:space:]]+} from '%s'" symbol package-name)
            nil
            t)))
    (message "skip import")
    (save-excursion
      (goto-char 0)
      (insert (concat "import { " symbol " } from '" package-name "';\n")))))


;;----------------------------------------------------------------------------
;; Node insert import default
;;----------------------------------------------------------------------------
(defun node-insert-import-default-if-not-found (symbol package-name)
  "Add import statement at the top of the js/ts buffer, if given symbol not exists"
  (if (save-excursion
        (save-match-data
          (goto-char (point-min))
          (re-search-forward
            (format "import %s from '%s'" symbol package-name)
            nil
            t)))
    (message "skip import")
    (save-excursion
      (goto-char 0)
      (insert (concat "import " symbol " from '" package-name "';\n")))))

;;----------------------------------------------------------------------------
;; Node insert import all as
;;----------------------------------------------------------------------------
(defun node-insert-import-all-if-not-found (symbol package-name)
  "Add import statement at the top of the js/ts buffer, if given symbol not exists"
  (if (save-excursion
        (save-match-data
          (goto-char (point-min))
          (re-search-forward
            (format "import * as %s from '%s'" symbol package-name)
            nil
            t)))
    (message "skip import")
    (save-excursion
      (goto-char 0)
      (insert (concat "import * as " symbol " from '" package-name "';\n")))))

;;----------------------------------------------------------------------------
;; Node insert import
;;----------------------------------------------------------------------------
(defun node-insert-import (symbol package-name)
  "Add import statement at the top of the js/ts buffer"
  (save-excursion
    (goto-char 0)
    (insert (concat "import { " symbol " } from '" package-name "';\n"))))

;;----------------------------------------------------------------------------
;; Show memory usage
;;----------------------------------------------------------------------------
(defun memory-usage ()
  "Show memory usage."
  (interactive)
  (message "%s" (shell-command-to-string (concat "ps -p " (format "%s" (emacs-pid)) " -xm -o %mem,rss,comm"))))


(defun airpod ()
  "Display airpod status."
  (interactive)
  (let ((script
          "#!/bin/bash
# AirPods Battery CLI, Version 2.3
# Contributors: duk242, ankushg, spetykowski, danozdotnet
# Released under the MIT License.

OUTPUT='🎧';
BLUETOOTH_DEFAULTS=$(defaults read /Library/Preferences/com.apple.Bluetooth); SYSTEM_PROFILER=$(system_profiler SPBluetoothDataType 2>/dev/null)
MAC_ADDR=$(grep -b2 \"Minor Type: Headphones\"<<<\"${SYSTEM_PROFILER}\"|awk '/Address/{print $3}')
# echo $MAC_ADDR 04-FE-A1-4A-59-77
# MAC_ADDR=\"b8-5d-0a-56-97-c4\"
CONNECTED=$(grep -ia6 \"${MAC_ADDR}\"<<<\"${SYSTEM_PROFILER}\"|awk '/Connected: Yes/{print 1}')
BLUETOOTH_DATA=$(grep -ia6 '\"'\"${MAC_ADDR}\"'\"'<<<\"${BLUETOOTH_DEFAULTS}\")
BATTERY_LEVELS=(\"BatteryPercentCombined\" \"HeadsetBattery\" \"BatteryPercentSingle\" \"BatteryPercentCase\" \"BatteryPercentLeft\" \"BatteryPercentRight\")

if [[ \"${CONNECTED}\" ]]; then
  for I in \"${BATTERY_LEVELS[@]}\"; do
    declare -x \"${I}\"=\"$(awk -v pat=\"${I}\" '$0~pat{gsub (\";\",\"\"); print $3 }'<<<\"${BLUETOOTH_DATA}\")\"
    [[ ! -z \"${!I}\" ]] && OUTPUT=\"${OUTPUT} $(awk '/BatteryPercent/{print substr($0,15,1)\": \"}'<<<\"${I}\")${!I}%\"
  done
  printf \"%s\\n\" \"${OUTPUT}\"
else
  printf \"%s Not Connected\\n\" \"${OUTPUT}\"
fi
"))
    (shell-command (format "bash -c %s" (shell-quote-argument script)))))

(defun set-frame-size-maximize-mac ()
  "Set frame size to fit default mac resolution."
  (interactive)
  (progn
    (set-frame-width (selected-frame) 253)
    (set-frame-height (selected-frame) 59)))

(defun new-frame-and-maximize ()
  "Create new frame and maximize it."
  (interactive)
  (progn
    (make-frame)
    (set-frame-width (selected-frame) 253)
    (set-frame-height (selected-frame) 59)))

(defun til-commit ()
  "Auto commit TIL."
  (interactive)
  (message "til index && til commit: 🔥")
  (async-shell-command "til index && til commit"))

(defun til-pull ()
  "Update TIL."
  (interactive)
  (message "til pull: 🔥")
  (async-shell-command "til pull"))

(defun til-open ()
  "Open TIL github."
  (interactive)
  (message (shell-command-to-string "til open")))

(defun keyboard-layout-to-us ()
  "Create new frame and maximize it.
https://github.com/myshov/xkbswitch-macosx"
  (interactive)
  (shell-command "$HOME/repo/github.com/myshov/xkbswitch-macosx/bin/xkbswitch -s 0")
  (message "Set input source US"))

(defun copy-current-path ()
  "Copy current path to clipboard."
  (interactive)
  (let* ((path (if (buffer-file-name) (buffer-file-name) (expand-file-name default-directory))))
    (kill-new path)
    (message "Copied %s" path)))

(defun open-clipboard-path ()
  "Open path from clipboard."
  (interactive)
  (let* ((path (current-kill 0)))
    (cond
      ((file-directory-p path) (find-file path))
      ((file-exists-p path) (find-file path))
      (t (message "Clipboard data is not a valid path: %s" (substring path 0 (min 100 (length path)))))
    )))

(defun finder ()
  "Open finder in current position"
  (interactive)
  (call-process-shell-command "open" nil nil nil (expand-file-name default-directory)))

(defun eslint-fix-file ()
  "Fix file with eslint."
  (interactive)
  (message "pnpm eslint --fix %s" (buffer-file-name))
  (shell-command (concat "pnpm eslint --fix " (buffer-file-name))))

(defun eslint-fix-file-and-revert ()
  "Fix file with eslint and revert."
  (interactive)
  (eslint-fix-file)
  (revert-buffer t t))

(defun wrap-region-parenthesis-to-function ()
  "Wrap region and move cursor to front and be input mode."
  (interactive)
  (save-excursion
    (goto-char (region-end))
    (insert ")"))
  (goto-char (region-beginning))
  (insert "(")
  (backward-char 1)
  (evil-insert-state 1))

(provide 'init-utils)

;;; init-utils.el ends here



