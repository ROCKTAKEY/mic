;;; mic.el --- Minimal configuration manager  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: convenience

;; Version: 0.11.2
;; Package-Requires: ((emacs "26.1"))
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

(defmacro mic-setappend (var val)
  "Append and set VAL to VAR."
  `(setq ,var (append ,var ,val)))

(defsubst mic-make-sexp-autoload-interactive (name list)
  "Create `autoload' sexp from LIST and NAME.
Each element of LIST is interactive function which should be autoloaded,
and NAME is feature."
  (mapcar
   (lambda (arg)
     `(autoload #',arg ,(symbol-name name) nil t))
   list))

(defsubst mic-make-sexp-autoload-noninteractive (name list)
  "Create `autoload' sexp from LIST and NAME.
Each element of LIST is non-interactive function which should be autoloaded,
and NAME is feature."
  (mapcar
   (lambda (arg)
     `(autoload #',arg ,(symbol-name name)))
   list))

(defsubst mic-make-sexp-custom (alist)
  "Create `customize-set-variable' sexp from ALIST.
`car' of each element is SYMBOL, and `cdr' is VALUE."
  (mapcar
   (lambda (arg)
     `(customize-set-variable
       ',(car arg)
       ,(cdr arg)))
   alist))

(defsubst mic-make-sexp-declare-function (name list)
  "Create `declare-function' sexp from LIST and NAME.
Each element of LIST is function which should be declared."
  (mapcar
   (lambda (arg)
     `(declare-function ,arg ,(concat "ext:" (symbol-name name))))
   list))

(defsubst mic-make-sexp-define-key (alist)
  "Create `define-key' sexp from ALIST.
`car' of each element is keymap, and `cdr' is list whose element is
\(KEY-STRING . COMMAND)."
  (cl-mapcan
   (lambda (keymap-binds-alist)
     (let ((keymap (car keymap-binds-alist))
           (binds (cdr keymap-binds-alist)))
       (mapcar
        (lambda (bind)
          (let ((key (car bind))
                (command (cdr bind)))
            `(define-key ,keymap (kbd ,key) ,command)))
        binds)))
   alist))

(defsubst mic-make-sexp-define-key-with-feature (alist)
  "Create `define-key' sexp from ALIST with `with-eval-after-load'.
`car' of each element of ALIST is FEATURE.
`cdr' of each element of ALIST is same as ALIST of `mic-make-sexp-define-key'."
  (mapcar
   (lambda (cons)
     (append (list #'with-eval-after-load `',(car cons))
             (mic-make-sexp-define-key (cdr cons))))
   alist))

(defsubst mic-make-sexp-defvar-noninitial (list)
  "Create `defvar' sexp from LIST and NAME.
Each element of LIST is variable which should be declared."
  (mapcar
   (lambda (arg)
     `(defvar ,arg))
   list))

(defsubst mic-make-sexp-face (alist)
  "Create `custom-set-faces' sexp from ALIST.
`car' of each element is FACE, and `cdr' is VALUE."
  (and alist
       `((custom-set-faces
          ,@(mapcan
             (lambda (arg)
               `('(,(car arg)
                   ,(cdr arg))))
             alist)))))

(defsubst mic-make-sexp-hook (alist)
  "Create `add-hook' sexp from ALIST.
`car' of each element is HOOK, and `cdr' is FUNCTION."
  (mapcar
   (lambda (arg)
     `(add-hook ',(car arg) ,(cdr arg)))
   alist))

(defsubst mic-make-sexp-package (list)
  "Create `package-install' sexp from LIST.
Each element is package symbol."
  (mapcar
   (lambda (arg)
     `(unless (package-installed-p ',arg)
        (package-install ',arg)))
   list))



(defmacro mic--plist-put (plist prop val)
  "Same as `plist-put', but fine when PLIST is nil.
Change value in PLIST of PROP to VAL."
  `(if ,plist
       (plist-put ,plist ,prop ,val)
     (setq ,plist (list ,prop ,val))))

(defmacro mic--plist-put-append (plist prop val)
  "Append VAL to value in PLIST of PROP."
  `(if ,plist
       (plist-put ,plist ,prop (append (plist-get ,plist ,prop) ,val))
     (setq ,plist (list ,prop (append (plist-get ,plist ,prop) ,val)))))

(defmacro mic--plist-delete (plist &rest props)
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



(defmacro mic-deffilter-const (name &optional docstring &rest plist)
  "Define filter macro named NAME with document DOCSTRING.
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
             (push `(mic--plist-put plist ,key ,value) result)))
         (nreverse result))
     plist))

(defmacro mic-deffilter-const-append (name &optional docstring &rest plist)
  "Define filter macro named NAME with document DOCSTRING.
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
             (push `(mic--plist-put-append plist ,key ,value) result)))
         (nreverse result))
     plist))

(cl-defmacro mic (name &key
                       autoload-intaractive
                       autoload-nonintaractive
                       custom
                       custom-after-load
                       declare-function
                       define-key
                       define-key-after-load
                       define-key-with-feature
                       defvar-noninitial
                       eval
                       eval-after-load
                       eval-after-others
                       eval-after-others-after-load
                       eval-before-all
                       face
                       hook
                       package)
  "Manage configuration of paackage named NAME.

Optional argument AUTOLOAD-INTARACTIVE, AUTOLOAD-NONINTARACTIVE, CUSTOM,
CUSTOM-AFTER-LOAD, DECLARE-FUNCTION, DEFINE-KEY, DEFINE-KEY-AFTER-LOAD,
DEFINE-KEY-WITH-FEATURE, DEFVAR-NONINITIAL, EVAL, EVAL-AFTER-LOAD,
EVAL-AFTER-OTHERS, EVAL-AFTER-OTHERS-AFTER-LOAD, EVAL-BEFORE-ALL, FACE, HOOK,
PACKAGE."
  (declare (indent defun))
  ;; AUTOLOAD-INTERACTIVE
  (let ((sexp (mic-make-sexp-autoload-interactive name autoload-intaractive)))
    (mic-setappend eval sexp))

  ;; AUTOLOAD-NONINTERACTIVE
  (let ((sexp (mic-make-sexp-autoload-noninteractive name autoload-nonintaractive)))
    (mic-setappend eval sexp))

  ;; CUSTOM
  (let ((sexp (mic-make-sexp-custom custom)))
    (mic-setappend eval sexp))

  ;; CUSTOM-AFTER-LOAD
  (let ((sexp (mic-make-sexp-custom custom-after-load)))
    (mic-setappend eval-after-load sexp))

  ;; DECLARE-FUNCTION
  (let ((sexp (mic-make-sexp-declare-function name declare-function)))
    (mic-setappend eval sexp))

  ;; DEFINE-KEY
  (let ((sexp (mic-make-sexp-define-key define-key)))
    (mic-setappend eval sexp))

  ;; DEFINE-KEY-AFTER-LOAD
  (let ((sexp (mic-make-sexp-define-key define-key-after-load)))
    (mic-setappend eval-after-load sexp))

  ;; DEFINE-KEY-WITH-FEATURE
  (let ((sexp (mic-make-sexp-define-key-with-feature define-key-with-feature)))
    (mic-setappend eval sexp))

  ;; DEFVAR-NONINITIAL
  (let ((sexp (mic-make-sexp-defvar-noninitial defvar-noninitial)))
    (mic-setappend eval sexp))

  ;; FACE
  (let ((sexp (mic-make-sexp-face face)))
    (mic-setappend eval sexp))

  ;; HOOK
  (let ((sexp (mic-make-sexp-hook hook)))
    (mic-setappend eval sexp))

  ;; PACKAGE
  (let ((sexp (mic-make-sexp-package package)))
    (mic-setappend eval-before-all sexp))

  ;; Result
  `(prog1 ',name
     ,@eval-before-all
     ,@eval
     ,@eval-after-others
     ,@(and (or eval-after-load
                eval-after-others-after-load)
            (list
             (append
              (list 'with-eval-after-load `',name)
              eval-after-load
              eval-after-others-after-load)))))

(provide 'mic)
;;; mic.el ends here
