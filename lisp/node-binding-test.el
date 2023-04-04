;;; Package --- node-binding-test
;;; Code:

;;; M-x load-file RET node-binding-test.el RET
;;; M-x ert RET t RET

(require 'node-binding)

(ert-deftest node-ensure-in-typescript-sourcefile-test ()
  "Tests for `node-ensure-in-typescript-sourcefile` function."
  (should-error (node-ensure-in-typesciprt-sourcefile))
  (should-error (progn
                  (find-file "test.ts")
                  (node-ensure-in-typesciprt-sourcefile))))

;;; node-binding-test.el ends here
