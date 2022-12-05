;;; init-typescript-extended.el --- Completion with company -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'tide)

(defun my-tide-refactor ()
  "Refactor code at point or current region."
  (interactive)
  (let ((response (tide-command:getApplicableRefactors)))
    (tide-on-response-success response (:min-version "2.4")
      (-if-let (body (plist-get response :body))
        (tide-apply-refactor (my-tide-select-refactor body))
        (message "No refactors available.")))))

(defun my-tide-select-refactor (applicable-refactor-infos)
  (let ((available-refactors
          (-mapcat
            (lambda (applicable-refactor-info)
              (-map (lambda (refactor-action-info)
                      `(:action ,(plist-get refactor-action-info :name)
                         :refactor ,(plist-get applicable-refactor-info :name)
                         :inlineable ,(plist-get applicable-refactor-info :inlineable)
                         :description ,(plist-get refactor-action-info :description)))
                (plist-get applicable-refactor-info :actions)))
            applicable-refactor-infos)))
    (tide-select-ivy "refactor" available-refactors #'tide-get-refactor-description)))

(defun tide-select-ivy (title candidates label-fn)
  "Select refactor CANDIDATES with ivy buffer and show TITLE.
LABEL-FN to pick description"
  (let ((collection (make-hash-table :test 'equal)))
    (dolist (item candidates)
      (puthash (funcall label-fn item) item collection))
    (let* ((selected-text (ivy-read (format "Select %s: " title) (hash-table-keys collection)))
            (selected-candidate (gethash selected-text collection)))
      (if (null selected-candidate)
        (error (concat (format "No %s selected: " title) selected-text))
        selected-candidate))))

(defun my-tide-fix (&optional arg)
  "Apply code fix for the error at point.

When invoked with a prefix arg, apply code fix for all the errors
in the file that are similar to the error at point."
  (interactive "P")
  (if arg
      (my-tide-code-fix #'my-tide-apply-codefix-for-all-in-file)
    (my-tide-code-fix #'tide-apply-codefix)))

(defun my-tide-code-fix (codefix-fn)
  (unless (tide-get-flycheck-errors-ids-at-point)
    (error "No errors available at current point."))
  (let ((response (tide-command:getCodeFixes)))
    (tide-on-response-success response
      (let ((fixes (plist-get response :body)))
        (my-tide-apply-codefixes fixes codefix-fn)))))

(defun my-tide-apply-codefix-for-all-in-file (fix)
  (tide-apply-codefix fix)
  (-when-let* ((fix-id (plist-get fix :fixId))
               (response (tide-command:getCombinedCodeFix fix-id)))
    (tide-on-response-success response
      (tide-apply-codefix (plist-get response :body)))))

(defun my-tide-apply-codefixes (fixes codefix-fn)
  (cond ((= 0 (length fixes)) (message "No code-fixes available."))
        ((= 1 (length fixes)) (funcall codefix-fn (car fixes)))
        (t (funcall
            codefix-fn
            (tide-select-ivy "fix" fixes #'tide-get-fix-description)))))


(provide 'init-typescript-extended)
;;; init-typescript-extended.el ends here
