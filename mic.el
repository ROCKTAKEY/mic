;;; mic.el --- Minimal configuration manager  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: convenience

;; Version: 0.6.0
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

(defmacro mic-setappend (var val)
  "Append and set VAL to VAR."
  `(setq ,var (append ,var ,val)))

(defsubst mic-make-sexp-custom (alist)
  "Create `customize-set-variable' sexp from ALIST.
`car' of each element is SYMBOL, and `cdr' is VALUE."
  (mapcar
   (lambda (arg)
     `(customize-set-variable
       ',(car arg)
       ,(cdr arg)))
   alist))

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

(defsubst mic-make-sexp-hook (alist)
  "Create `add-hook' sexp from ALIST.
`car' of each element is HOOK, and `cdr' is FUNCTION."
  (mapcar
   (lambda (arg)
     `(add-hook ',(car arg) ,(cdr arg)))
   alist))

(cl-defmacro mic (name &key
                       autoload-intaractive
                       autoload-nonintaractive
                       custom
                       custom-after-load
                       define-key
                       define-key-after-load
                       eval
                       eval-after-load
                       hook)
  "Manage configuration of paackage named NAME.

Optional argument CUSTOM, CUSTOM-AFTER-LOAD, DEFINE-KEY, DEFINE-KEY-AFTER-LOAD,
EVAL, EVAL-AFTER-LOAD, HOOK."
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

  ;; DEFINE-KEY
  (let ((sexp (mic-make-sexp-define-key define-key)))
    (mic-setappend eval sexp))

  ;; DEFINE-KEY-AFTER-LOAD
  (let ((sexp (mic-make-sexp-define-key define-key-after-load)))
    (mic-setappend eval-after-load sexp))

  ;; HOOK
  (let ((sexp (mic-make-sexp-hook hook)))
    (mic-setappend eval sexp))

  ;; Result
  `(prog1 ',name
     ,@eval
     ,@(and eval-after-load
            (list
             (append
              (list 'with-eval-after-load `',name)
              eval-after-load)))))

(provide 'mic)
;;; mic.el ends here
