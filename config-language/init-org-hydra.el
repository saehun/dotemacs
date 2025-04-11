;;; init-org-hydra.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; 
;;;; Requirements:
(require 'org)
(require 'org-element-ast)
(require 'consult-org-roam)
(require 'hydra)

;;; Code:


(defhydra hydra-org-default (:color pink :hint nil :exit t)
  "

  Org default hydra
  
  _C-t_ insert timestamp                _C-f_ forward links        _n_ new node at point  _f_ format lines
  _b_   make indirect buffer from tree  _C-b_ backward links       _g_ save to github
  _q_ quit                              _y_ set id and store link  _t_ insert tag
"
  ("b" org-tree-to-indirect-buffer)
  ("C-t" org-insert-now-timestamp)
  ("C-f" consult-org-roam-forward-links)
  ("C-b" consult-org-roam-backlinks)
  ("y" org-id-store-link)
  ("n" org-roam-node-insert-immediate)
  ("g" my/org-commit)
  ("f" org-fill-paragraph)
  ("t" org-roam-tag-add)
  ("q" nil :color blue))


(defhydra hydra-org-link-helper (:color pink :hint nil)
  "

  org link helper
  
  _n_ next link
  _p_ previous link
  _q_ quit
"
  ("n" org-next-link)
  ("p" org-previous-link)
  ("q" nil :color blue))

(defhydra hydra-org-table-helper (:color pink :hint nil)
  "

  org table helper
  
  _r_ recalculate     _w_ wrap region      _c_ toggle coordinates
  _i_ iterate table   _t_ transpose        _D_ toggle debugger
  _B_ iterate buffer  _E_ export table     _d_ edit field
  _e_ eval formula    _s_ sort lines
  _q_ quit
"
  ("E" org-table-export :color blue)
  ("s" org-table-sort-lines)
  ("d" org-table-edit-field)
  ("e" org-table-eval-formula)
  ("r" org-table-recalculate)
  ("i" org-table-iterate)
  ("B" org-table-iterate-buffer-tables)
  ("w" org-table-wrap-region)
  ("D" org-table-toggle-formula-debugger)
  ("t" org-table-transpose-table-at-point)
  ("c" org-table-toggle-coordinate-overlays :color blue)
  ("q" nil :color blue))

(defhydra hydra-babel-helper (:color pink :hint nil)
  "

 org babel src block helper functions
 
 _n_ next       _i_ info           _I_ insert header
 _p_ prev       _c_ check
 _h_ goto head  _E_ expand
 ^ ^            _s_ split
 _q_ quit       _r_ remove result  _e_ examplify region
"
  ("i" org-babel-view-src-block-info)
  ("I" org-babel-insert-header-arg)
  ("c" org-babel-check-src-block :color blue)
  ("s" org-babel-demarcate-block :color blue)
  ("n" org-babel-next-src-block)
  ("p" org-babel-previous-src-block)
  ("E" org-babel-expand-src-block :color blue)
  ("e" org-babel-examplify-region :color blue)
  ("r" org-babel-remove-result :color blue)
  ("h" org-babel-goto-src-block-head)
  ("q" nil :color blue))


;; Hydra
(defun org-hydra-launcher ()
  "A launcher for hydras based on the current context."
  (interactive)
  (if (eq major-mode 'org-mode)
      (let* ((elem (org-element-context))
             (etype (car elem))
             (type (org-element-property :type elem)))
        (cl-case etype
          (src-block (hydra-babel-helper/body))
          (link (hydra-org-link-helper/body))
          ((table-row table-cell) (hydra-org-table-helper/body) )
          ;; (t (message "No specific hydra for %s/%s" etype type)
          (t (hydra-org-default/body))))
    (message "Not in org-mode")))


(define-key org-mode-map (kbd "C-e") 'org-hydra-launcher)


(message "init-org-hydra.el")
(provide 'init-org-hydra)
;;; init-org-hydra.el ends here
