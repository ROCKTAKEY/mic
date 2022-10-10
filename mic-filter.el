;;; mic-filter.el --- Filter definitions for mic  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>

;;; Commentary:

;; Filter definitions for `mic'.

;;; Code:
(require 'mic)

(defun mic-filter-straight (plist)
  "Create `straight-use-package' sexp from PLIST and append to value of :eval."
  (mic-plist-put-append
   plist :eval
   (mapcar
    (lambda (arg)
      `(straight-use-package ',arg))
    (plist-get plist :straight)))
  (mic-plist-delete plist :straight))

(defun mic-filter-el-get (plist)
  "Create `el-get-bundle' sexp from PLIST and append to value of :eval.
This filter use :el-get keyword."
  (mic-plist-put-append
   plist :eval
   (mapcar
    (lambda (arg)
      `(el-get-bundle ,@(if (listp arg) arg (list arg))))
    (plist-get plist :el-get)))
  (mic-plist-delete plist :el-get))

(provide 'mic-filter)
;;; mic-filter.el ends here
