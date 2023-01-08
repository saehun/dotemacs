;;; package --- init-hydra-typescript
;;; Commentary:
;;; Code:

(require 'hydra)
(require 'init-utils)
(require 'web-mode)

(defhydra hydra-typescript (:color pink
                             :hint nil)
  "

  ^Command^         ^Watch^
  ^^^^^^^^--------------------------
  _r_: run          _w_: watch
  _R_: debug run    _W_: watch test
  _t_: test
  _T_: debug test

"
  ("r" node/run-current-file)
  ("R" node/debug-current-file)
  ("t" node/test-current-file)
  ("T" node/debug-test-current-file)
  ("w" node/watch-run-current-file)
  ("W" node/watch-test-current-file)

  ("c" nil "cancel")
  ("v" Buffer-menu-select "select" :color blue)
  ("o" Buffer-menu-other-window "other-window" :color blue)
  ("q" quit-window "quit" :color blue))

(define-key web-mode-map (kbd "s-e") 'hydra-typescript/body)

(provide 'init-hydra-typescript)

;;; init-hydra-typescript.el ends here
