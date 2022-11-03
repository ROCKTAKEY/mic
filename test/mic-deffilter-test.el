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

(mic-deffilter-alias mic-test-mic-deffilter-alias
  :foo :baz)

(ert-deftest mic-deffilter-alias ()
  (let* ((init '(:foo 1 :bar 2))
         (result (mic-test-mic-deffilter-alias init)))
    (should (equal (plist-get result :foo) nil))
    (should (equal (plist-get result :baz) 1))
    (should (equal (plist-get result :bar) 2))))

(mic-deffilter-const mic-test-mic-deffilter-const
  :foo t
  :bar '(2 4))

(ert-deftest mic-deffilter-const ()
  (let* ((init '(:foo 1 :bar 2))
         (result (mic-test-mic-deffilter-const init)))
    (should (equal (plist-get result :foo) t))
    (should (equal (plist-get result :bar) '(2 4)))))

(mic-deffilter-const-append mic-test-mic-deffilter-const-append
  :foo '(t)
  :bar '(3 4))

(ert-deftest mic-deffilter-const-append ()
  (let* ((init '(:foo (1) :bar (2)))
         (result (mic-test-mic-deffilter-const-append init)))
    (should (equal (plist-get result :foo) '(1 t)))
    (should (equal (plist-get result :bar) '(2 3 4)))))

(mic-deffilter-ignore mic-test-mic-deffilter-ignore
  :foo)

(ert-deftest mic-deffilter-ignore ()
  (let* ((init '(:foo (1) :bar (2)))
         (result (mic-test-mic-deffilter-ignore init)))
    (should (equal result '(:bar (2)))))

  (let* ((init '(:bar (2) :foo (1)))
         (result (mic-test-mic-deffilter-ignore init)))
    (should (equal result '(:bar (2))))))

(mic-deffilter-nonlist-to-list mic-test-mic-deffilter-nonlist-to-list :foo)

(ert-deftest mic-deffilter-nonlist-to-list ()
  (let* ((init '(:foo (1 t x) :bar (4)))
         (result (mic-test-mic-deffilter-nonlist-to-list init)))
    (should (equal (plist-get result :foo) '(1 t x)))
    (should (equal (plist-get result :bar) '(4))))

  (let* ((init '(:foo a :bar (4)))
         (result (mic-test-mic-deffilter-nonlist-to-list init)))
    (should (equal (plist-get result :foo) '(a)))
    (should (equal (plist-get result :bar) '(4)))))

(mic-deffilter-t-to-name mic-test-mic-deffilter-t-to-name :foo)

(ert-deftest mic-deffilter-t-to-name ()
  (let* ((init '(:name name :foo (1 t x) :bar (4)))
         (result (mic-test-mic-deffilter-t-to-name init)))
    (should (equal (plist-get result :foo) '(1 name x)))
    (should (equal (plist-get result :bar) '(4)))))

(mic-deffilter-validate mic-test-mic-deffilter-validate
  :foo :bar)

(ert-deftest mic-deffilter-validate ()
  (let* ((init '(:foo t :bar 2))
         (result (mic-test-mic-deffilter-validate init)))
    (should (equal (plist-get result :foo) t))
    (should (equal (plist-get result :bar) 2)))

  (let* ((init '(:foo 1 :hoge 2)))
    (cl-letf (((symbol-function 'warn) (lambda (&rest arg) (error "Error"))))
      (should-error (mic-test-mic-deffilter-validate init)))))

(provide 'mic-deffilter-test)
;;; mic-deffilter-test.el ends here
