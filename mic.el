;;; mic.el --- Minimal configuration manager  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: convenience

;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1") (cl-lib "0.7"))
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

(cl-defmacro mic (&key custom)
  "Minimal configuration manager.

Optional argument CUSTOM."
  `(progn
     ,@(mapcar
        (lambda (arg)
          `(customize-set-variable
            ',(car arg)
            ,(cdr arg)))
        custom)))

(provide 'mic)
;;; mic.el ends here
