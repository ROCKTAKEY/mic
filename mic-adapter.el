;;; mic-adapter.el --- Adapters for mic

;; Copyright (C) 2023 by ROCKTAKEY

;; Author: ROCKTAKEY <ROCKTAKEY@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Adapters for mic

;;; Code:

(require 'mic-utils)

(defun mic-adapter-use-package (plist)
  "An adapter from `mic-core'-like input PLIST to `use-package'.
It takes one argument PLIST, and transforms it into `use-package' sexp."
  (let ((eval (plist-get plist :eval))
        ;; Variable named `eval-after-load' is warned by `package-lint', so use `eal' instead.
        (eal (plist-get plist :eval-after-load))
        (eval-after-others (plist-get plist :eval-after-others))
        (eval-after-others-after-load (plist-get plist :eval-after-others-after-load))
        (eval-before-all (plist-get plist :eval-before-all))
        (eval-installation (plist-get plist :eval-installation))
        (rest-plist plist))
    (mic-plist-delete rest-plist
                      :eval
                      :eval-after-load
                      :eval-after-others
                      :eval-after-others-after-load
                      :eval-before-all
                      :eval-installation
                      :defer)
    `(,@rest-plist
      :defer ,(if (plist-member plist :defer) (plist-get plist :defer) t)
      :preface
      ,@eval-before-all
      ,@eval-installation
      :init
      ,@eval
      ,@eval-after-others
      :config
      ,@eal
      ,@eval-after-others-after-load)))

(defun mic-adapter-leaf (plist)
  "An adapter from `mic-core'-like input PLIST to `leaf'.
It takes one argument PLIST, and transforms it into `leaf' sexp."
  (let ((eval (plist-get plist :eval))
        ;; Variable named `eval-after-load' is warned by `package-lint', so use `eal' instead.
        (eal (plist-get plist :eval-after-load))
        (eval-after-others (plist-get plist :eval-after-others))
        (eval-after-others-after-load (plist-get plist :eval-after-others-after-load))
        (eval-before-all (plist-get plist :eval-before-all))
        (eval-installation (plist-get plist :eval-installation))
        (rest-plist plist))
    (mic-plist-delete rest-plist
                      :eval
                      :eval-after-load
                      :eval-after-others
                      :eval-after-others-after-load
                      :eval-before-all
                      :eval-installation)
    `(,@rest-plist
      :preface
      ,@eval-before-all
      ,@eval-installation
      :init
      ,@eval
      ,@eval-after-others
      ;; Sometimes :config is not wrapped around `with-eval-after-load',
      ;; so use :defer-config instead.
      :defer-config
      ,@eal
      ,@eval-after-others-after-load)))

(provide 'mic-adapter)

;;; mic-adapter.el ends here
