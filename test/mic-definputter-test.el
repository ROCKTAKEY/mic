;;; mic-definputter-test.el --- test for mic-definputter

;; Copyright (C) 2023  ROCKTAKEY

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

(require 'mic-definputter)

(mic-definputter-pseudo-plist inputter-eval
  '(:eval :eval-after-load))

(ert-deftest mic-definputter-pseudo-plist ()
  (should
   (equal
    (inputter-eval '(:eval
                     (message "Hello")
                     (message "Hi")
                     (message "Good bye")
                     :eval-after-load
                     (message "Good afternoon")
                     (message "Good evening")
                     :require (cl-lib)))

    '(:eval
      ((message "Hello")
       (message "Hi")
       (message "Good bye"))
      :eval-after-load
      ((message "Good afternoon")
       (message "Good evening"))
      :require (cl-lib)))))

(provide 'mic-definputter-test)
;;; mic-definputter-test.el ends here
