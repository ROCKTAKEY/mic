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



(provide 'mic-filter)
;;; mic-filter.el ends here
