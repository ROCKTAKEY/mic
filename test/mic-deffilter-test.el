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

(mic-ert-macroexpand-1 mic-deffilter-const-macroexpand-1
  ((mic-deffilter-const func-name
     :foo t
     :bar '(2 4))
   . (defun func-name (plist)
       "Filter for `mic'.
It return PLIST but each value on some property below is replaced:
(:foo t :bar
      '(2 4))
"
       (mic-plist-put plist :foo t)
       (mic-plist-put plist :bar '(2 4))
       plist))
  ((mic-deffilter-const func-name
     "docstring"
     :foo t
     :bar '(2 4))
   . (defun func-name (plist)
       "docstring"
       (mic-plist-put plist :foo t)
       (mic-plist-put plist :bar '(2 4))
       plist)))

(ert-deftest mic-deffilter-const ()
  (mic-deffilter-const mic-test-mic-deffilter-const
    :foo t
    :bar '(2 4))

  (let* ((init '(:foo 1 :bar 2))
         (result (mic-test-mic-deffilter-const init)))
    (should (equal (plist-get result :foo) t))
    (should (equal (plist-get result :bar) '(2 4)))))

(mic-ert-macroexpand-1 mic-deffilter-const-append-macroexpand-1
  ((mic-deffilter-const-append func-name
     :foo '(t)
     :bar '(2 4))
   . (defun func-name (plist)
       "Filter for `mic'.
It return PLIST but each value on some property below is appended:
(:foo
 '(t)
 :bar
 '(2 4))
"
       (mic-plist-put-append plist :foo
                              '(t))
       (mic-plist-put-append plist :bar
                              '(2 4))
       plist))
  ((mic-deffilter-const-append func-name
     "docstring"
     :foo '(t)
     :bar '(2 4))
   . (defun func-name (plist)
       "docstring"
       (mic-plist-put-append plist :foo '(t))
       (mic-plist-put-append plist :bar '(2 4))
       plist)))

(ert-deftest mic-deffilter-const-append ()
  (mic-deffilter-const-append mic-test-mic-deffilter-const-append
    :foo '(t)
    :bar '(3 4))

  (let* ((init '(:foo (1) :bar (2)))
         (result (mic-test-mic-deffilter-const-append init)))
    (should (equal (plist-get result :foo) '(1 t)))
    (should (equal (plist-get result :bar) '(2 3 4)))))

(provide 'mic-deffilter-test)
;;; mic-deffilter-test.el ends here
