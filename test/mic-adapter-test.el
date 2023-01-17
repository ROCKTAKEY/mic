;;; mic-adapter-test.el --- test for mic-adapter

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

(require 'mic-adapter)

(ert-deftest mic-adapter-use-package ()
  (should
   (equal
    (mic-adapter-use-package '( :eval ((message "eval"))
                                :eval-after-load ((message "eval-after-load"))
                                :eval-after-others ((message "eval-after-others"))
                                :eval-after-others-after-load ((message "eval-after-others-after-load"))
                                :eval-before-all ((message "eval-before-all"))
                                :eval-installation ((message "eval-installation"))))
    '( :defer t
       :preface
       (message "eval-before-all")
       (message "eval-installation")
       :init
       (message "eval")
       (message "eval-after-others")
       :config
       (message "eval-after-load")
       (message "eval-after-others-after-load"))))

  (should
   (equal
    (mic-adapter-use-package '( :eval ((message "eval"))
                                :eval-after-load ((message "eval-after-load"))
                                :eval-after-others ((message "eval-after-others"))
                                :eval-after-others-after-load ((message "eval-after-others-after-load"))
                                :eval-before-all ((message "eval-before-all"))
                                :eval-installation ((message "eval-installation"))
                                :defer nil))
    '( :defer nil
       :preface
       (message "eval-before-all")
       (message "eval-installation")
       :init
       (message "eval")
       (message "eval-after-others")
       :config
       (message "eval-after-load")
       (message "eval-after-others-after-load"))))

  (should
   (equal
    (mic-adapter-use-package '( :eval ((message "eval"))
                                :eval-after-load ((message "eval-after-load"))
                                :eval-after-others ((message "eval-after-others"))
                                :eval-after-others-after-load ((message "eval-after-others-after-load"))
                                :eval-before-all ((message "eval-before-all"))
                                :eval-installation ((message "eval-installation"))
                                :bind (("M-a" . beginning-of-defun))))
    '( :bind (("M-a" . beginning-of-defun))
       :defer t
       :preface
       (message "eval-before-all")
       (message "eval-installation")
       :init
       (message "eval")
       (message "eval-after-others")
       :config
       (message "eval-after-load")
       (message "eval-after-others-after-load")))))

(ert-deftest mic-adapter-leaf ()
  (should
   (equal
    (mic-adapter-leaf '( :eval ((message "eval"))
                                :eval-after-load ((message "eval-after-load"))
                                :eval-after-others ((message "eval-after-others"))
                                :eval-after-others-after-load ((message "eval-after-others-after-load"))
                                :eval-before-all ((message "eval-before-all"))
                                :eval-installation ((message "eval-installation"))))
    '( :preface
       (message "eval-before-all")
       (message "eval-installation")
       :init
       (message "eval")
       (message "eval-after-others")
       :defer-config
       (message "eval-after-load")
       (message "eval-after-others-after-load"))))

  (should
   (equal
    (mic-adapter-leaf '( :eval ((message "eval"))
                                :eval-after-load ((message "eval-after-load"))
                                :eval-after-others ((message "eval-after-others"))
                                :eval-after-others-after-load ((message "eval-after-others-after-load"))
                                :eval-before-all ((message "eval-before-all"))
                                :eval-installation ((message "eval-installation"))
                                :bind (("M-a" . beginning-of-defun))))
    '( :bind (("M-a" . beginning-of-defun))
       :preface
       (message "eval-before-all")
       (message "eval-installation")
       :init
       (message "eval")
       (message "eval-after-others")
       :defer-config
       (message "eval-after-load")
       (message "eval-after-others-after-load")))))



(provide 'mic-adapter-test)
;;; mic-adapter-test.el ends here
