(require 'f)

(defvar mocha-test/test-path
  (f-parent (f-this-file)))

(defvar mocha-test/root-path
  (f-parent mocha-test/test-path))

(require 'ert)
(require 'mocha (f-expand "mocha" mocha-test/root-path))
