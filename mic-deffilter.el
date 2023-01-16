;;; mic-deffilter.el --- Definer of filter for mic   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>

;;; Commentary:

;; Definer of filter for mic

;;; Code:

(require 'cl-lib)
(require 'mic-utils)

;;;###autoload
(defmacro mic-deffilter-alias (name  alias target &optional docstring)
  "Define filter function named NAME with document DOCSTRING.
The filter recieves plist and returns plist.
It make alias named ALIAS to TARGET."
  (declare (indent defun))
  (let ((key (make-symbol "key"))
        (value (make-symbol "value"))
        (result (make-symbol "result")))
   `(defun ,name (plist)
     ,(or docstring
          (format "Filter for `mic'.
It return PLIST but value on %s is replaced with %s."
                  alias target))
     (let (,result)
       (while plist
         (let ((,key (pop plist))
               (,value (pop plist)))
           (when (eq ,key ,alias)
             (setq ,key ,target))
           (push ,key ,result)
           (push ,value ,result)))
       (nreverse ,result)))))

;;;###autoload
(defmacro mic-deffilter-const (name &optional docstring &rest plist)
  "Define filter function named NAME with document DOCSTRING.
The filter recieves plist and returns plist.
It replace value on each property in PLIST with each value in PLIST."
  (declare (indent defun))
  (unless (stringp docstring)
    (push docstring plist)
    (setq docstring nil))
  `(defun ,name (plist)
     ,(or docstring
          (format "Filter for `mic'.
It return PLIST but each value on some property below is replaced:
%s" (pp-to-string plist)))
     ,@(let (result)
         (while plist
           (let ((key (pop plist))
                 (value (pop plist)))
             (push `(mic-plist-put plist ,key ,value) result)))
         (nreverse result))
     plist))

;;;###autoload
(defmacro mic-deffilter-const-append (name &optional docstring &rest plist)
  "Define filter function named NAME with document DOCSTRING.
The filter recieves plist and returns plist.
It append each value in PLIST on each property to recieved plist."
  (declare (indent defun))
  (unless (stringp docstring)
    (push docstring plist)
    (setq docstring nil))
  `(defun ,name (plist)
     ,(or docstring
          (format "Filter for `mic'.
It return PLIST but each value on some property below is appended:
%s" (pp-to-string plist)))
     ,@(let (result)
         (while plist
           (let ((key (pop plist))
                 (value (pop plist)))
             (push `(mic-plist-put-append plist ,key ,value) result)))
         (nreverse result))
     plist))

;;;###autoload
(defmacro mic-deffilter-ignore (name keyword &optional docstring)
  "Define filter function named NAME with document DOCSTRING.
The filter recieves plist and returns plist.
If the value on KEYWORD in PLIST exists, remove it."
  (declare (indent defun))
  `(defun ,name (plist)
     ,(or docstring
          (format "Filter for `mic'.
If the value on %s in PLIST exists, remove it."
                  keyword))
     (cl-remf plist ,keyword)
     plist))

;;;###autoload
(defmacro mic-deffilter-nonlist-to-list (name keyword &optional docstring)
  "Define filter function named NAME with document DOCSTRING.
The filter recieves plist and returns plist.
If the value on KEYWORD in PLIST is not list NON-LIST,
replace it with `(NON-LIST)'."
  (declare (indent defun))
  (let ((value (make-symbol "value")))
    `(defun ,name (plist)
       ,(or docstring
            (format "Filter for `mic'.
If the value on %s in PLIST is symbol SYMBOL, replace to '(SYMBOL)."
                    keyword))
       (let ((,value (plist-get plist ,keyword)))
         (unless (listp ,value)
           (mic-plist-put
            plist
            ,keyword
            (list ,value)))
         plist))))

;;;###autoload
(defmacro mic-deffilter-replace-keyword-append
    (name filter new-keyword old-keyword replacement-alist &optional docstring)
  "Define filter function named NAME with document DOCSTRING.
The filter recieves PLIST and returns plist.
The filter conduct same procedure as FILTER, but input and output keyword is
altered.
1. NEW-KEYWORD is used as an input keyword instead of OLD-KEYWORD.
2. Output keywords are replaced according to REPLACEMENT-ALIST.
  Each `cdr' keyword is replaced with `car' keyword in REPLACEMENT-ALIST."
  (declare (indent defun))
  `(defun ,name (plist)
     ,(or docstring
          (format "Filter for `mic'.
Almost same as `%s' but input keyword is `%s' instead of `%s',
and output is replaced according to a alist:
%s
where each `cdr' keyword is replaced with `car'."
                  filter new-keyword old-keyword replacement-alist))
     (let ((inner-output-plist (,filter (list ,old-keyword (plist-get plist ,new-keyword)))))
       (mic-plist-replace-keywords inner-output-plist ,replacement-alist)
       (while inner-output-plist
         (let ((key (pop inner-output-plist))
               (value (pop inner-output-plist)))
           (mic-plist-put-append plist
                                 key value))))
     (mic-plist-delete plist ,new-keyword)))

;;;###autoload
(defmacro mic-deffilter-convert-after-load
    (name filter new-keyword old-keyword &optional docstring)
  "Define filter function named NAME with document DOCSTRING.
This macro makes FILTER `after-load'-ized.

The filter recieves PLIST and returns plist.
The filter conduct same procedure as FILTER, but input and output keyword is
altered.
1. NEW-KEYWORD is used as an input keyword instead of OLD-KEYWORD.
2. `:eval' is replaced with `:eval-after-load' in output."
  (declare (indent defun))
  `(mic-deffilter-replace-keyword-append
    ,name ,filter ,new-keyword ,old-keyword
    '((:eval . :eval-after-load))
    ,docstring))

;;;###autoload
(defmacro mic-deffilter-t-to-name (name keyword &optional docstring)
  "Define filter function named NAME with document DOCSTRING.
The filter recieves plist and returns plist.
It replaces element t of the list value on KEYWORD in PLIST
to the value on `:name'."
  (declare (indent defun))
  `(defun ,name (plist)
     ,(or docstring
          (format "Filter for `mic'.
It return PLIST but element t of the list value on %s
is replaced to the value on `:name'."
                  keyword))
     (mic-plist-put
      plist
      ,keyword
      (mapcar
       (lambda (arg)
         (if (eq arg t)
             (plist-get plist :name)
           arg))
       (plist-get plist ,keyword)))))

;;;###autoload
(defmacro mic-deffilter-validate (name &optional docstring &rest keywords)
  "Define filter function named NAME with document DOCSTRING.
The filter recieves PLIST and return PLIST.
It validates PLIST properties and warn if PLIST has invalid properties.
KEYWORDS is a list of valid properties."
  (declare (indent defun))
  (unless (stringp docstring)
    (push docstring keywords)
    (setq docstring nil))
  (let ((result (make-symbol "result"))
        (key (make-symbol "key"))
        (value (make-symbol "value")))
    `(defun ,name (plist)
       ,(or docstring
            (format "Filter for `mic'.
It validates PLIST properties and warn if PLIST has invalid properties.
The valid properties are:
%s" (mapconcat
     (lambda (arg)
       (concat "`" (symbol-name arg) "'"))
     keywords
     "\n")))
       (let (,result)
         (while plist
           (let ((,key (pop plist))
                 (,value (pop plist)))
             (if (not (memq ,key (append ',keywords '(:name))))
                 (warn "`mic' %s: The keyword %s is not allowed by filter `%s'" (plist-get plist :name) ,key ',name)
               (push ,key ,result)
               (push ,value ,result))))
         (nreverse ,result)))))

(provide 'mic-deffilter)
;;; mic-deffilter.el ends here
