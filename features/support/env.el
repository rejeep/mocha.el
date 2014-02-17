(require 'f)

(defvar mocha-test/support-path
  (f-dirname (f-this-file)))

(defvar mocha-test/features-path
  (f-parent mocha-test/support-path))

(defvar mocha-test/root-path
  (f-parent mocha-test/features-path))

(add-to-list 'load-path mocha-test/root-path)

(require 'mocha)
(require 'espuds)
(require 'ert)
