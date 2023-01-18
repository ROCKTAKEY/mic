;;; mic-definputter.el --- Inputter for `mic'  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 by ROCKTAKEY

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Inputter for `mic'

;;; Code:

(require 'mic-utils)

(defmacro mic-definputter-pseudo-plist (name listized-keywords &optional docstring)
  "Define an inputter, named NAME and documented by DOCSTRING, for `mic-defmic'.
Here is description for an inputter defined by this.
---
Any keywords in LISTIZED-KEYWORDS are allowed to put muptiple value,
whereas the other keywords are not allowed.

For example, when LISTIZED-KEYWORDS is (:a :b), lists below are allowed:
 (:a 1
  :b 4 5 6
  :c 7)
 (:a 1 2
  :c 7)
 (:a 1
  :b 2 4
  :a 5
  :c 9)
but lists below are not allowed:
 (:a 1 2
  :c 3 4) ; Because :c is not allowed to be multiple.
 (:a 1
  :c 2
  :b 4
  :c 4) ; Because :c is not allowed to be multiple.

This function returns plist whose value on keyword in LISTIZED-KEYWORDS is
put together into one list.

For example, when LISTIZED-KEYWORDS is (:a :b),
lists below are transformed like:
 (:a 1
  :b 4 5 6
  :c 7)
 ;; =>
 (:a (1)
  :b (4 5 6)
  :c 7)"
  (declare (indent defun))
  (let ((listized-keywords (eval listized-keywords)))
    `(defun ,name (pseudo-plist)
       ,(or docstring
            (format "Inputter for `mic'.
It takes one argument PSEUDO-PLIST and return normalized plist.
Keywords listed below are allowed to put muptiple value,
whereas the other keywords are not allowed:
  %s
For more information, see `mic-definputter-pseudo-plist'."
                    (mapconcat
                     (lambda (arg)
                       (concat "- `" (pp-to-string arg) "'"))
                     listized-keywords
                     "\n")))
       (let (redundant-plist)
         (let (key)
           (while pseudo-plist
             (let ((now (pop pseudo-plist)))
               (if (keywordp now)
                   (setq key now)
                 (mic-plist-put-append redundant-plist key (list now))))))
         (let (normalized-plist)
           (while redundant-plist
             (let ((key (pop redundant-plist))
                   (value (pop redundant-plist)))
               (mic-plist-put normalized-plist key
                              (if (seq-some
                                   (apply-partially #'eq key)
                                   ',listized-keywords)
                                  value
                                (when (cdr value)
                                  (error "`%s': keyword `%s' is not allowed to be passed multiple times"
                                         ',name key))
                                (car value)))))
           normalized-plist)))))

(provide 'mic-definputter)

;;; mic-definputter.el ends here
