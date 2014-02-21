(Given "^I create directory \"\\([^\"]+\\)\"$"
  (lambda (directory)
    (f-mkdir (f-expand directory mocha-test/sandbox-path))))

(Given "^I create file \"\\([^\"]+\\)\" with contents:$"
  (lambda (file contents)
    (f-write-text contents 'utf-8 (f-expand file mocha-test/sandbox-path))))

(Given "^I delete \"\\([^\"]+\\)\"$"
  (lambda (path)
    (f-delete (f-expand path mocha-test/sandbox-path) 'force)))

(When "^I visit file \"\\([^\"]+\\)\"$"
  (lambda (file)
    (find-file (f-expand file mocha-test/sandbox-path))))

(When "^I run mocha$"
  (lambda ()
    (condition-case err
        (mocha)
      (error
       (setq mocha-test/last-error (error-message-string err))))))

(Then "^I should see buffer \"\\([^\"]+\\)\"$"
  (lambda (buffer-name)
    (let ((buffer-names (-map 'buffer-name (-map 'window-buffer (window-list)))))
      (should (-contains? buffer-names buffer-name)))))

(Then "^I should see contents in buffer \"\\([^\"]+\\)\":$"
  (lambda (buffer contents)
    (with-current-buffer buffer
      (should (s-contains? contents (buffer-substring-no-properties (point-min) (point-max)))))))

(Then "^I should see error \"\\([^\"]+\\)\"$"
  (lambda (message)
    (should (string= message mocha-test/last-error))))
