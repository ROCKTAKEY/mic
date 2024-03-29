;;; mic-utils.el --- Utility for mic                 -*- lexical-binding: t; -*-

;; Copyright (C) 2022  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>

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

;; Utility for mic

;;; Code:

(require 'cl-lib)

;;;###autoload
(defmacro mic-plist-put (plist prop val)
  "Same as `plist-put', but fine when PLIST is nil.
Change value in PLIST of PROP to VAL."
  `(setq ,plist
         (if ,plist
             (plist-put ,plist ,prop ,val)
           (list ,prop ,val))))

;;;###autoload
(defmacro mic-plist-put-append (plist prop val)
  "Append VAL to value in PLIST of PROP."
  `(when ,val
     (setq ,plist
           (if ,plist
               (plist-put ,plist ,prop (append (plist-get ,plist ,prop) ,val))
             (list ,prop (append (plist-get ,plist ,prop) ,val))))))

;;;###autoload
(defmacro mic-plist-delete (plist &rest props)
  "Delete PROPS and their values from PLIST."
  (let ((original-plist (cl-gensym "plist"))
        (result (cl-gensym "result"))
        (key (cl-gensym "key"))
        (value (cl-gensym "value")))
    `(let ((,original-plist ,plist)
           ,result)
       (while ,original-plist
         (let ((,key (pop ,original-plist))
               (,value (pop ,original-plist)))
           (unless (memq ,key ',props)
             (push ,key ,result)
             (push ,value ,result))))
       (setq ,plist (nreverse ,result)))))

;;;###autoload
(defun mic-plist-replace-keywords (plist replacement-alist)
  "Replace each keyword in PLIST.
Each element of REPLACEMENT-ALIST is (FROM . TO),
where keyword FROM is replaced with keyword TO."
  (mapc
   (lambda (cons)
     (let ((from (car cons))
           (to (cdr cons)))
       (if-let ((value (plist-member plist from)))
           (setcar value to))))
   replacement-alist)
  plist)

(provide 'mic-utils)
;;; mic-utils.el ends here
