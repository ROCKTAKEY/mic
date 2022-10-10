;;; mic-filter-test.el --- test for mic-filter

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

(require 'mic-filter)

(ert-deftest mic-filter-straight ()
  (should (equal (mic-filter-straight '(:straight (x y (emacs-ja
                                                        :host github
                                                        :repo "ayatakesi/ayatakesi.github.io"
                                                        :files ("emacs/26.1/emacs-ja.info")))))
                 '(:eval ((straight-use-package 'x)
                          (straight-use-package 'y)
                          (straight-use-package
                           '(emacs-ja :host github :repo "ayatakesi/ayatakesi.github.io"
                                      :files ("emacs/26.1/emacs-ja.info")))))))
  (should (eq (plist-get (mic-filter-straight '( :straight (x y (emacs-ja
                                                                 :host github
                                                                 :repo "ayatakesi/ayatakesi.github.io"
                                                                 :files ("emacs/26.1/emacs-ja.info")))
                                                 :foo bar))
                         :foo)
              'bar)))

(ert-deftest mic-filter-el-get ()
  (should (equal (mic-filter-el-get '(:el-get (x y (emacs-ja
                                                    :host github
                                                    :repo "ayatakesi/ayatakesi.github.io"
                                                    :files ("emacs/26.1/emacs-ja.info")))))
                 '(:eval ((el-get-bundle x)
                          (el-get-bundle y)
                          (el-get-bundle
                            emacs-ja :host github :repo "ayatakesi/ayatakesi.github.io"
                            :files ("emacs/26.1/emacs-ja.info"))))))
  (should (eq (plist-get (mic-filter-straight '( :straight (x y (emacs-ja
                                                                 :host github
                                                                 :repo "ayatakesi/ayatakesi.github.io"
                                                                 :files ("emacs/26.1/emacs-ja.info")))
                                                 :foo bar))
                         :foo)
              'bar)))

(provide 'mic-filter-test)
;;; mic-filter-test.el ends here
