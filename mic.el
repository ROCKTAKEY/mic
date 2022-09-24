;;; mic.el --- Minimal configuration manager  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: convenience

;; Version: 0.12.0
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
(require 'subr-x)

(defgroup mic ()
  "Minimal configuration manager."
  :group 'convenience
  :prefix "mic-"
  :link '(url-link "https://github.com/ROCKTAKEY/mic"))

(defmacro mic-setappend (var val)
  "Append and set VAL to VAR."
  `(setq ,var (append ,var ,val)))

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



(defmacro mic-apply-filter (plist name &rest filters)
  "Apply FILTERS to PLIST.
NAME is temporarily added to PLIST on :name property."
  (declare (indent defun))
  `(progn
     (mic--plist-put ,plist :name ,name)
     (setq ,plist (thread-last
                    ,plist
                    ,@filters))
     (mic--plist-delete ,plist :name)))

(cl-defmacro mic-defmic (name parent docstring &rest plist)
  "Define new `mic' named NAME derived from PARENT.
DOCSTRING is docuent of it.
FILTERS is list of filter, which recieve plist and return plist.
The recieved plist has property `:name', which is package name.
It also has other properties from other filter before.

The defined macro recieves two arguments, NAME and PLIST.
PLIST is filtered by each FILTERS in order and passed to PARENT.

\(fn NAME PARENT [DOCSTRING] &key FILTERS)"
  (declare (indent defun)
           (doc-string 2))
  (unless (stringp docstring)
    (push docstring plist)
    (setq docstring nil))
  (let ((filters (eval (plist-get plist :filters))))
    `(defmacro ,name (name &rest plist)
       ,(or docstring
            (format "`mic' alternative defined by `mic-defmic'.
Argument NAME, PLIST. Used filters are:
%s"
                    (mapconcat
                     (lambda (arg)
                       (concat "- `" (pp-to-string arg) "'"))
                     filters
                     "\n")))
       (declare (indent defun))
       (mic-apply-filter plist name
         ,@filters)
       (backquote
        ,(list
          parent
          ',name
          ',@plist)))))



(defsubst mic-make-sexp-autoload-interactive (name list)
  "Create `autoload' sexp from LIST and NAME.
Each element of LIST is interactive function which should be autoloaded,
and NAME is feature."
  (mapcar
   (lambda (arg)
     `(autoload #',arg ,(symbol-name name) nil t))
   list))

(defun mic-filter-autoload-interactive (plist)
  "Append sexp from value :autoload-interactive to value of :eval on PLIST.
The sexp is generated by `mic-make-sexp-autoload-interactive'."
  (mic--plist-put-append plist :eval
                         (mic-make-sexp-autoload-interactive
                          (plist-get plist :name)
                          (plist-get plist :autoload-interactive)))
  (mic--plist-delete plist :autoload-interactive)
  plist)

(defsubst mic-make-sexp-autoload-noninteractive (name list)
  "Create `autoload' sexp from LIST and NAME.
Each element of LIST is non-interactive function which should be autoloaded,
and NAME is feature."
  (mapcar
   (lambda (arg)
     `(autoload #',arg ,(symbol-name name)))
   list))

(defun mic-filter-autoload-noninteractive (plist)
  "Append sexp from value of :autoload-noninteractive to value of :eval on PLIST.
The sexp is generated by `mic-make-sexp-autoload-noninteractive'."
  (mic--plist-put-append plist :eval
                         (mic-make-sexp-autoload-noninteractive
                          (plist-get plist :name)
                          (plist-get plist :autoload-noninteractive)))
  (mic--plist-delete plist :autoload-noninteractive)
  plist)

(defsubst mic-make-sexp-custom (alist)
  "Create `customize-set-variable' sexp from ALIST.
`car' of each element is SYMBOL, and `cdr' is VALUE."
  (mapcar
   (lambda (arg)
     `(customize-set-variable
       ',(car arg)
       ,(cdr arg)))
   alist))

(defun mic-filter-custom (plist)
  "Append sexp from value of :custom to value of :eval on PLIST.
Sexp is generated by `mic-make-sexp-custom'."
  (mic--plist-put-append plist :eval
                         (mic-make-sexp-custom
                          (plist-get plist :custom)))
  (mic--plist-delete plist :custom)
  plist)

(defun mic-filter-custom-after-load (plist)
  "Append sexp from value of :custom-afer-load to value of :eval-after-load.
PLIST should be property list.
Sexp is generated by `mic-make-sexp-custom'."
  (mic--plist-put-append plist :eval-after-load
                         (mic-make-sexp-custom
                          (plist-get plist :custom-after-load)))
  (mic--plist-delete plist :custom-after-load)
  plist)

(defsubst mic-make-sexp-declare-function (name list)
  "Create `declare-function' sexp from LIST and NAME.
Each element of LIST is function which should be declared."
  (mapcar
   (lambda (arg)
     `(declare-function ,arg ,(concat "ext:" (symbol-name name))))
   list))

(defun mic-filter-declare-function (plist)
  "Append sexp from value of :declare-function to value of :eval on PLIST.
Sexp is generated by `mic-make-sexp-declare-function'."
  (mic--plist-put-append plist :eval
                         (mic-make-sexp-declare-function
                          (plist-get plist :name)
                          (plist-get plist :declare-function)))
  (mic--plist-delete plist :declare-function)
  plist)

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

(defun mic-filter-define-key (plist)
  "Append sexp from value of :define-key to value of :eval on PLIST.
Sexp is generated by `mic-make-sexp-define-key'."
  (mic--plist-put-append plist :eval
                         (mic-make-sexp-define-key
                          (plist-get plist :define-key)))
  (mic--plist-delete plist :define-key)
  plist)

(defun mic-filter-define-key-after-load (plist)
  "Append sexp from value of :define-key-after-load to value of :eval-after-load.
PLIST should be property list.
Sexp is generated by `mic-make-sexp-define-key-after-load'."
  (mic--plist-put-append plist :eval-after-load
                         (mic-make-sexp-define-key
                          (plist-get plist :define-key-after-load)))
  (mic--plist-delete plist :define-key-after-load)
  plist)

(defsubst mic-make-sexp-define-key-with-feature (alist)
  "Create `define-key' sexp from ALIST with `with-eval-after-load'.
`car' of each element of ALIST is FEATURE.
`cdr' of each element of ALIST is same as ALIST of `mic-make-sexp-define-key'."
  (mapcar
   (lambda (cons)
     (append (list #'with-eval-after-load `',(car cons))
             (mic-make-sexp-define-key (cdr cons))))
   alist))

(defun mic-filter-define-key-with-feature (plist)
  "Append sexp from value of :define-key-with-feature to value of :eval on PLIST.
Sexp is generated by `mic-make-sexp-define-key-with-feature'."
  (mic--plist-put-append plist :eval
                         (mic-make-sexp-define-key-with-feature
                          (plist-get plist :define-key-with-feature)))
  (mic--plist-delete plist :define-key-with-feature)
  plist)

(defsubst mic-make-sexp-defvar-noninitial (list)
  "Create `defvar' sexp from LIST and NAME.
Each element of LIST is variable which should be declared."
  (mapcar
   (lambda (arg)
     `(defvar ,arg))
   list))

(defun mic-filter-defvar-noninitial (plist)
  "Append sexp from value of :defvar-noninitial to value of :eval on PLIST.
Sexp is generated by `mic-make-sexp-defvar-noninitial'."
  (mic--plist-put-append plist :eval
                         (mic-make-sexp-defvar-noninitial
                          (plist-get plist :defvar-noninitial)))
  (mic--plist-delete plist :defvar-noninitial)
  plist)

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

(defun mic-filter-face (plist)
  "Append sexp from value of :face to value of :eval on PLIST.
Sexp is generated by `mic-make-sexp-face'."
  (mic--plist-put-append plist :eval
                         (mic-make-sexp-face
                          (plist-get plist :face)))
  (mic--plist-delete plist :face)
  plist)

(defsubst mic-make-sexp-hook (alist)
  "Create `add-hook' sexp from ALIST.
`car' of each element is HOOK, and `cdr' is FUNCTION."
  (mapcar
   (lambda (arg)
     `(add-hook ',(car arg) ,(cdr arg)))
   alist))

(defun mic-filter-hook (plist)
  "Append sexp from value of :hook to value of :eval on PLIST.
Sexp is generated by `mic-make-sexp-hook'."
  (mic--plist-put-append plist :eval
                         (mic-make-sexp-hook
                          (plist-get plist :hook)))
  (mic--plist-delete plist :hook)
  plist)

(defsubst mic-make-sexp-package (list)
  "Create `package-install' sexp from LIST.
Each element is package symbol."
  (mapcar
   (lambda (arg)
     `(unless (package-installed-p ',arg)
        (package-install ',arg)))
   list))

(defun mic-filter-package (plist)
  "Append sexp from value of :package to value of :eval on PLIST.
Sexp is generated by `mic-make-sexp-package'."
  (mic--plist-put-append plist :eval
                         (mic-make-sexp-package
                          (plist-get plist :package)))
  (mic--plist-delete plist :package)
  plist)



(cl-defmacro mic-core (name &key
                            eval
                            eval-after-load
                            eval-after-others
                            eval-after-others-after-load
                            eval-before-all)
  "Configuration package named NAME.
It evaluates each element of EVAL-BEFORE-ALL, EVAL, EVAL-AFTER-OTHERS in order.
In addition, It will evaluate each element of EVAL-AFTER-LOAD and
 EVAL-AFTER-OTHERS-AFTER-LOAD after load of package NAME."
  (declare (indent defun))
  `(prog1 ',name
     ,@(and (or eval-after-load
                eval-after-others-after-load)
            (list
             (append
              (list 'with-eval-after-load `',name)
              eval-after-load
              eval-after-others-after-load)))
     ,@eval-before-all
     ,@eval
     ,@eval-after-others))

(mic-defmic mic mic-core
  :filters
  '(mic-filter-autoload-interactive
    mic-filter-autoload-noninteractive
    mic-filter-custom
    mic-filter-custom-after-load
    mic-filter-declare-function
    mic-filter-define-key
    mic-filter-define-key-after-load
    mic-filter-define-key-with-feature
    mic-filter-defvar-noninitial
    mic-filter-face
    mic-filter-hook
    mic-filter-package))

(provide 'mic)
;;; mic.el ends here
