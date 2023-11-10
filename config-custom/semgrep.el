;;; semgrep.el --- Semgrep support for Emacs         -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Ruin0x11

;; Author: Ruin0x11 <ipickering2@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Package-Requires: ((swiper "0.12.0") (ivy "0.12.0") (emacs "24.1") (projectile "2.3.0"))
;;; Commentary:

;; Allows running Semgrep from Emacs using ivy. You can specify a pattern/rules
;; file to use, or run using the current project's .semgrep.yml file.

;;; Code:
(require 'swiper)
(require 'projectile)

(defgroup counsel-semgrep nil
  "Semgrep support for Emacs."
  :group 'ivy
  :group 'semgrep)

(defcustom semgrep-executable-name "semgrep" "Executable to use for semgrep.")

(defvar semgrep-executable-arguments '("--emacs"))
(defvar semgrep-run-language nil)
(defvar semgrep-pattern-history '())
(defvar semgrep-language-history '())

;;;###autoload
(defun semgrep (pattern lang target &optional args)
  "Run a semgrep search with `PATTERN' on `TARGET'.
`ARGS' provides semgrep command line arguments."
  (interactive
   (list (read-from-minibuffer "Semgrep search for: " nil nil nil 'semgrep-pattern-history)
         (read-from-minibuffer "Language: " (car semgrep-language-history) nil nil '(semgrep-language-history . 1))
         (read-directory-name "Directory: ")))
  (setq semgrep-last-language lang)
  (compile
   (mapconcat 'identity
              (append (list semgrep-executable-name)
                      semgrep-executable-arguments
                      args
                      (list "-l" lang "-e" (shell-quote-argument pattern) target)) " ")))

;;;###autoload
(defun semgrep-file (&optional file target args)
  "Run semgrep with rules `FILE' on `TARGET' with `ARGS'."
  (interactive
   (list
    (read-file-name "Semgrep rules file: ")
    (read-directory-name "Directory: ")))
  (compile
   (mapconcat 'identity
              (append (list semgrep-executable-name)
                      semgrep-executable-arguments
                      args
                      (when file (list "-f" file))
                      (list target)) " ")))

;;;###autoload
(defun projectile-semgrep-pattern (pattern lang)
  "Run a semgrep search with `PATTERN' of `LANG' rooted at the current projectile project root."
  (interactive
   (list
    (read-from-minibuffer (projectile-prepend-project-name "Semgrep search for: ")
                          nil nil nil 'semgrep-pattern-history)
    (read-from-minibuffer "Language: " (car semgrep-language-history) nil nil '(semgrep-language-history . 1))))
  (let ((args (mapcar (lambda (val) (concat "--exclude " val))
                      (append (projectile-ignored-files-rel)
                              (projectile-ignored-directories-rel)))))
    (semgrep pattern
             lang
             (projectile-project-root)
             args)))

;;;###autoload
(defun projectile-semgrep (&optional target)
  "Run default semgrep on project directory."
  (interactive
   (list
    (when current-prefix-arg (read-file-name "Target file or directory: "))))
  (let ((args (mapcar (lambda (val) (concat "--exclude " val))
                      (append (projectile-ignored-files-rel)
                              (projectile-ignored-directories-rel)))))
    (let ((default-directory (projectile-project-root)))
      (semgrep-file nil (or target (projectile-project-root)) args))))

(provide 'semgrep)
;;; semgrep.el ends here
