;;; mic-utils.el --- Utility for mic                 -*- lexical-binding: t; -*-

;; Copyright (C) 2022  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>

;;; Commentary:

;; Utility for mic

;;; Code:

(require 'cl-lib)

;;;###autoload
(defmacro mic-plist-put (plist prop val)
  "Same as `plist-put', but fine when PLIST is nil.
Change value in PLIST of PROP to VAL."
  `(if ,plist
       (plist-put ,plist ,prop ,val)
     (setq ,plist (list ,prop ,val))))

;;;###autoload
(defmacro mic-plist-put-append (plist prop val)
  "Append VAL to value in PLIST of PROP."
  `(if ,plist
       (plist-put ,plist ,prop (append (plist-get ,plist ,prop) ,val))
     (setq ,plist (list ,prop (append (plist-get ,plist ,prop) ,val)))))

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

(provide 'mic-utils)
;;; mic-utils.el ends here
