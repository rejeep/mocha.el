;;; mocha.el --- Emacs minor mode for Mocha.js

;; Copyright (C) 2014 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.1
;; Keywords: test, javascript, mocha
;; URL: http://github.com/rejeep/mocha.el
;; Package-Requires: ((s "1.9.0") (dash "2.5.0") (f "0.16.0") (nvm "0.0.3") (emacs "24"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'f)
(require 's)
(require 'dash)
(require 'nvm)


;;;; Variables

(defvar mocha-executable nil
  "Path to Mocha executable.")

(defvar mocha-reporters '(dot)
  "List of supported reporters.")

(defvar mocha-reporter 'dot
  "Reporter to use.")

(defvar mocha-use-nvm t
  "Whether or not to use Nvm.")


;;;; Functions

(defun mocha-colorize-compilation-buffer ()
  "Colorize compilation buffer."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))

(defun mocha-executable ()
  "Return path to Mocha executable."
  (or
   mocha-executable
   (executable-find
    (f-join (mocha-root-path) "node_modules" ".bin" "mocha"))
   (executable-find "mocha")))

(defun mocha-make-command (&rest args)
  "Return a Mocha command.

ARGS are appended at the end of the command."
  (s-join
   " "
   (append (list (mocha-executable)
                 (format "--reporter %s" mocha-reporter))
           args)))

(defun mocha-root-path ()
  "Return path to project root.

If root is not found, raise an error."
  (or
   (f-traverse-upwards
    (lambda (path)
      (f-exists? (f-expand "node_modules" path)))
    (f-parent (buffer-file-name)))
   (user-error "Could not determine project root, did not find any node_modules directory")))

(defun mocha-test-file-p (file)
  "Return t if FILE is a test file, nil otherwise."
  (or (s-contains? ".test" (f-filename file))
      (s-contains? ".spec" (f-filename file))))

(defun mocha-find-test-file (file)
  "Find test file associated with FILE."
  (let* ((path (f-relative file (f-expand "lib" (mocha-root-path))))
         (dir-path (f-dirname path))
         (filename (f-no-ext (f-filename path)))
         (test-path (f-join (mocha-root-path) "test" dir-path)))
    (car (f-glob (concat filename "*") test-path))))

(defun mocha-test-file (file)
  "Return test file associated with FILE."
  (if (mocha-test-file-p file)
      file
    (or (mocha-find-test-file file)
        (user-error "Did not find any matching test file for %s"
                    (f-relative file (mocha-root-path))))))

(defun mocha-enable-font-lock ()
  "Enable font lock to handle some characters."
  (font-lock-add-keywords
   'mocha-compilation-mode
   '(("\\(․\\)" (0 (prog1 nil
                     (compose-region
                      (match-beginning 1)
                      (match-end 1)
                      8729)))))))

(defun mocha-compile ()
  "Compile current file in `mocha-compilation-mode'."
  (let ((test-file (mocha-test-file (buffer-file-name))))
    (compile (mocha-make-command test-file) 'mocha-compilation-mode)))

;;;###autoload
(defun mocha ()
  "Run Mocha test for current file."
  (interactive)
  (let ((root-path (mocha-root-path)))
    (let ((default-directory (f-slash root-path)))
      (if mocha-use-nvm
          (if (f-file? (f-expand ".nvmrc" root-path))
              (nvm-use-for default-directory 'mocha-compile)
            (user-error "Using Nvm, but did not find any .nvmrc file"))
        (mocha-compile)))))

(define-derived-mode mocha-compilation-mode compilation-mode "Mocha Compilation"
  (add-hook 'compilation-filter-hook 'mocha-colorize-compilation-buffer nil t)
  (setq-local show-trailing-whitespace nil)
  (mocha-enable-font-lock))

(provide 'mocha)

;;; mocha.el ends here
