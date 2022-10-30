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
    (tide-select-refactor-ivy available-refactors #'tide-get-refactor-description)))

(defun tide-select-refactor-ivy (candidates label-fn)
  "Select refactor CANDIDATES with ivy buffer.
LABEL-FN to pick description"
  (let ((collection (make-hash-table :test 'equal)))
    (dolist (item candidates)
      (puthash (funcall label-fn item) item collection))
    (let* ((selected-text (ivy-read "Select refactor: " (hash-table-keys collection)))
            (selected-candidate (gethash selected-text collection)))
      (if (null selected-candidate)
        (error (concat "No refactor selected: " selected-text))
        selected-candidate))))

(provide 'init-typescript-extended)
;;; init-typescript-extended.el ends here
