;;; mic-utils-test.el --- test for mic-utils

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

(require 'mic-utils)

(ert-deftest mic-plist-put ()
  (let ((plist '(:foo 1 :bar 2)))
    (mic-plist-put plist :baz 3)
    (should (eq (plist-get plist :foo) 1))
    (should (eq (plist-get plist :bar) 2))
    (should (eq (plist-get plist :baz) 3)))

  (let (plist)
    (mic-plist-put plist :baz 3)
    (should (eq (plist-get plist :baz) 3))))

(ert-deftest mic-plist-put-append ()
  (let ((plist '(:foo 1 :bar 2)))
    (mic-plist-put-append plist :baz '(3))
    (should (eq (plist-get plist :foo) 1))
    (should (eq (plist-get plist :bar) 2))
    (should (equal (plist-get plist :baz) '(3))))

  (let ((plist '(:foo (1) :bar (2))))
    (mic-plist-put-append plist :bar '(3))
    (should (equal (plist-get plist :foo) '(1)))
    (should (equal (plist-get plist :bar) '(2 3))))

  (let (plist)
    (mic-plist-put-append plist :baz '(3))
    (should (equal (plist-get plist :baz) '(3)))))

(ert-deftest mic-plist-delete ()
  (let ((plist '(:foo 1 :bar 2)))
    (mic-plist-delete plist :foo)
    (should-not (plist-get plist :foo))
    (should (eq (plist-get plist :bar) 2))
    (should (equal plist '(:bar 2))))

  (let ((plist '(:foo 1 :bar 2)))
    (mic-plist-delete plist :bar)
    (should (eq (plist-get plist :foo) 1))
    (should-not (plist-get plist :bar))
    (should (equal plist '(:foo 1))))

  (let ((plist '(:foo 1 :bar 2 :baz 3)))
    (mic-plist-delete plist :bar :foo)
    (should-not (plist-get plist :foo))
    (should-not (plist-get plist :bar))
    (should (eq (plist-get plist :baz) 3))
    (should (equal plist '(:baz 3)))))

(provide 'mic-utils-test)
;;; mic-utils-test.el ends here
