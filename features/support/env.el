(require 'f)

(defvar mocha-test/support-path
  (f-dirname (f-this-file)))

(defvar mocha-test/features-path
  (f-parent mocha-test/support-path))

(defvar mocha-test/root-path
  (f-parent mocha-test/features-path))

(defvar mocha-test/sandbox-path
  (f-expand "sandbox" mocha-test/features-path))

(defvar mocha-test/buffer-list
  (buffer-list))

(defvar mocha-test/last-error nil)

(add-to-list 'load-path mocha-test/root-path)

(require 'mocha)
(require 'espuds)
(require 'ert)

(Before
 (setq mocha-test/last-error nil)

 (when (f-dir? mocha-test/sandbox-path)
   (f-delete mocha-test/sandbox-path 'force))
 (f-mkdir mocha-test/sandbox-path))

(After
 (--each (process-list) (signal-process it 'int)))
