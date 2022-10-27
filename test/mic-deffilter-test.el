;;; mic-deffilter-test.el --- test for mic-deffilter

;; Copyright (C) 2022  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords:

;; Version: 0.0.0
;; Package-Requires:
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'cl-lib)

(require 'undercover)
(undercover "*.el"
            (:report-format 'codecov)
            (:report-file "coverage-final.json")
            (:send-report nil))

(require 'mic-deffilter)

(defmacro mic-ert-macroexpand-1 (name &rest args)
  "Define test named NAME.
The test compare macro expandation of `car' of each element of ARGS with `cdr' of it.
The test defined by this expands macro once."
  (declare (indent defun))
  `(ert-deftest ,name ()
     ,@(mapcar
        (lambda (arg)
          `(should (equal (macroexpand-1 ',(car arg))
                          ',(cdr arg))))
        args)))



(ert-deftest mic-deffilter-const ()
  (mic-deffilter-const mic-test-mic-deffilter-const
    :foo t
    :bar '(2 4))

  (let* ((init '(:foo 1 :bar 2))
         (result (mic-test-mic-deffilter-const init)))
    (should (equal (plist-get result :foo) t))
    (should (equal (plist-get result :bar) '(2 4)))))

(ert-deftest mic-deffilter-const-append ()
  (mic-deffilter-const-append mic-test-mic-deffilter-const-append
    :foo '(t)
    :bar '(3 4))

  (let* ((init '(:foo (1) :bar (2)))
         (result (mic-test-mic-deffilter-const-append init)))
    (should (equal (plist-get result :foo) '(1 t)))
    (should (equal (plist-get result :bar) '(2 3 4)))))

(ert-deftest mic-deffilter-t-to-name ()
  (mic-deffilter-t-to-name mic-test-mic-deffilter-t-to-name :foo)

  (let* ((init '(:name name :foo (1 t x) :bar (4)))
         (result (mic-test-mic-deffilter-t-to-name init)))
    (should (equal (plist-get result :foo) '(1 name x)))
    (should (equal (plist-get result :bar) '(4)))))

(ert-deftest mic-deffilter-validate ()
  (mic-deffilter-validate mic-test-mic-deffilter-validate
    :foo :bar)

  (let* ((init '(:foo t :bar 2))
         (result (mic-test-mic-deffilter-validate init)))
    (should (equal (plist-get result :foo) t))
    (should (equal (plist-get result :bar) 2)))

  (let* ((init '(:foo 1 :hoge 2)))
    (cl-letf (((symbol-function 'warn) (lambda (&rest arg) (error "Error"))))
      (should-error (mic-test-mic-deffilter-validate init)))))

(provide 'mic-deffilter-test)
;;; mic-deffilter-test.el ends here
