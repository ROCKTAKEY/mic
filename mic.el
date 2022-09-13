;;; mic.el --- Minimal configuration manager  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: convenience

;; Version: 0.3.0
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/ROCKTAKEY/mic

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

;; Minimal configuration manager

;;; Code:

(require 'cl-lib)

(defgroup mic ()
  "Minimal configuration manager."
  :group 'convenience
  :prefix "mic-"
  :link '(url-link "https://github.com/ROCKTAKEY/mic"))

(cl-defmacro mic (name &key
                       custom
                       custom-after-load
                       eval
                       eval-after-load)
  "Minimal configuration manager.

Optional argument CUSTOM, NAME."
  (declare (indent defun))
  (let* ((sexp-custom (mapcar
                       (lambda (arg)
                         `(customize-set-variable
                           ',(car arg)
                           ,(cdr arg)))
                       custom))
         (sexp-custom-after-load (mapcar
                                  (lambda (arg)
                                    `(customize-set-variable
                                      ',(car arg)
                                      ,(cdr arg)))
                                  custom-after-load))

         (total-eval (append eval
                             sexp-custom))
         (total-eval-after-load (append eval-after-load
                                        sexp-custom-after-load)))
    `(prog1 ',name
       ,@total-eval
       ,@(and total-eval-after-load
              (list
               (append
                (list 'with-eval-after-load `',name)
                total-eval-after-load))))))

(provide 'mic)
;;; mic.el ends here
