;;; mic.el --- Minimal configuration manager  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: convenience

;; Version: 0.20.1
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
;;
;; [[https://github.com/ROCKTAKEY/mic][https://img.shields.io/github/tag/ROCKTAKEY/mic.svg?style=flat-square]]
;; [[file:LICENSE][https://img.shields.io/github/license/ROCKTAKEY/mic.svg?style=flat-square]]
;; [[https://codecov.io/gh/ROCKTAKEY/mic?branch`master][https://img.shields.io/codecov/c/github/ROCKTAKEY/mic.svg?style'flat-square]]
;; [[https://github.com/ROCKTAKEY/mic/actions][https://img.shields.io/github/workflow/status/ROCKTAKEY/mic/test/master.svg?style=flat-square]]
;;; mic: Minimal configuration manager for Emacs
;;
;; `mic' is uncustomizable.  Define your own `mic'.
;;
;;
;; `mic' is minimal configuration manager for Emacs.
;; This package is yet another `use-package' and `leaf', but is also used with them (See Alternative).
;; `mic' is minimal, so if you would like to write complex configuration,
;; `mic' is a little redundant.  However, there is no problem.  `mic' is thought to be
;; used to core to define your own, another, more convenient `mic'.
;; There are some functions to define your own `mic'.  See "Define your own mic".
;;
;;; How to Use?
;; For Emacs Lisp beginners, original `mic' macro is useful to configure your `init.el'.
;;
;; For Emacs Lisp expert, original `mic' is a little unsatisfactory or redundant.
;; `mic' is not customizable, but you can define your own `mic' easily.
;; 1. Determine parent.  You can use as parent `mic', `mic-core', which is simpler `mic'.
;;    `mic-core' recieves only keywords start from =:eval=, such as =:eval=, `eval-after-load'.
;; 2. Define filter functions.  Each one recieves plist (property list) and returns plist.
;;    returned plist is passed to parent (such as `mic', `mic-core') or next filter.
;;    Note that filter function can get feature name as value of property =:name=.
;;    Of course, you can use pre-defined filters.  `mic' is defined by some filters
;;    from the parent `mic-core'.
;; 3. Define your own mic by `mic-defmic'.  It recieves `NAME', optional `DOCSTRING',
;;    and keyword argument `FILTERS'.  `NAME' is name of your own `mic'.
;;    `DOCSTRING' is the document string of yours.  `FILTERS' are list of filter.
;;    As explained, filter recieves plist and returns plist.  It filter plist to get
;;    desired behavior.
;;
;;
;;   (defun my-filter-global-set-key-without-quote (plist)
;;     (let ((alist
;;            ;; Get value from your own keyword
;;            (plist-get plist :bind))
;;           sexps)
;;       (setq sexps
;;             ;; Transform each element
;;             (mapcar
;;              (lambda (arg)
;;                (let ((keys (car arg))
;;                      (command (cdr arg)))
;;                  `(global-set-key (kbd ,keys)
;;              alist))
;;       ;; Put sexps to `:eval' arguments
;;       (mic-plist-put-append plist :eval sexps)
;;       ;; Don't forget to delete your own keyword!
;;       ;; When forget it, parent recieves it and may cause unexpected result.
;;       (mic-plist-delete plist :bind)
;;       plist))
;;
;;   (mic-defmic mymic
;;     ;; Parent is here.  You can also use `mic-core'.
;;     mic
;;     :filters
;;     '(my-filter-global-set-key-without-quote
;;       ;; You can add other filters below
;;       ))
;;
;;   ;; Then you can use `mymic' like:
;;   (mymic simple
;;     :bind
;;     (("C-d" . delete-forward-char)
;;      ("C-x l" . toggle-truncate-lines))
;;     ;; Of course parent keywords are accepted.
;;     :custom
;;     ((kill-whole-line . t)
;;      (set-mark-command-repeat-pop . t)
;;      (mark-ring-max . 50)))
;;
;;   ;; `mymic' sexp is expanded to:
;;   (mic simple
;;     :custom
;;     ((kill-whole-line . t)
;;      (set-mark-command-repeat-pop . t)
;;      (mark-ring-max . 50))
;;     :eval
;;     ((global-set-key (kbd "C-d")
;;      (global-set-key (kbd "C-x l")
;;
;;   ;; Expanded to:
;;   (mic-core simple
;;     :eval
;;     ((global-set-key (kbd "C-d")
;;      (global-set-key (kbd "C-x l")
;;      (customize-set-variable 'kill-whole-line t)
;;      (customize-set-variable 'set-mark-command-repeat-pop t)
;;      (customize-set-variable 'mark-ring-max 50))
;;     :eval-after-load nil)
;;
;;   ;; Expanded to:
;;   (prog1 'simple
;;     (global-set-key  (kbd "C-d")
;;     (global-set-key (kbd "C-x l")
;;     (customize-set-variable 'kill-whole-line t)
;;     (customize-set-variable 'set-mark-command-repeat-pop t)
;;     (customize-set-variable 'mark-ring-max 50))
;;
;;
;;; Use `mic-core', minimum one
;; PROPERTIES:
;; CUSTOM_ID: mic-core
;; END:
;; `mic-core' is minimum.  It can recieves only 5 keywords:
;; - =:eval=
;; - =:eval-after-load=
;; - =:eval-after-others=
;; - =:eval-after-others-after-load=
;; - =:eval-before-all=
;;
;; Each element of =:eval= arguments are evaluated.
;; Time to evaluate is different.
;;
;;;; =:eval=, =:eval-after-others=, =:eval-before-all=
;; Each element of these arguments are evaluated when the `mic' sexp is evaluated.
;; The order is:
;; - =:eval-before-all=
;; - (`with-eval-after-load' sexp, explained on [[eval-after-load][`eval-after-load' keyword section]], is evaluated)
;; - =:eval=
;; - =:eval-after-others=
;;
;;
;;   (mic-core feature-name
;;     :eval
;;     ((message "eval1")
;;      (message "eval2"))
;;     :eval-after-others
;;     ((message "eval-after-others1")
;;      (message "eval-after-others2"))
;;     :eval-before-all
;;     ((message "eval-before-all1")
;;      (message "eval-before-all2"))
;;     :eval-after-load
;;     ((message "eval-after-load1")
;;      (message "eval-after-load2")))
;;
;;   ;; Expanded to:
;;   (prog1 'feature-name
;;     (message "eval-before-all1")
;;     (message "eval-before-all2")
;;     (with-eval-after-load 'feature-name
;;       (message "eval-after-load1")
;;       (message "eval-after-load2"))
;;     (message "eval1")
;;     (message "eval2")
;;     (message "eval-after-others1")
;;     (message "eval-after-others2"))
;;
;;
;; =:eval-before-all= exists because a filter function appends sexp to =:eval= argument.
;; When some action should be evaluated before all action added by other filters,
;; you can put it to =:eval-before-all= argument.  *Note that it should NOT be used
;; by filters.* Any filter should not use this.  If it is used by filters,
;; users cannot make their sexp to be evaluate before filter sexps.
;;
;; =:eval-after-others= exists because similar reason to =:eval-before-all=.
;; Some action should be evaluated after all action added by other filters.
;; Because of same reasons as =:eval-before-all=, *it should NOT be used
;; by filters*.
;;
;;;; =:eval-after-load=, =:eval-after-others-after-load=
;; PROPERTIES:
;; CUSTOM_ID: eval-after-load
;; END:
;; Each element of these arguments are evaluated after the package is loaded.
;; The evaluated order is:
;; - =:eval-after-load=
;; - =:eval-after-others-after-load=
;;
;;
;;   (mic-core feature-name
;;     :eval-after-load
;;     ((message "eval-after-load1")
;;      (message "eval-after-load2"))
;;     :eval-after-others-after-load
;;     ((message "eval-after-others-after-load1")
;;      (message "eval-after-others-after-load2")))
;;
;;   ;; Expanded to:
;;   (prog1 'feature-name
;;     (with-eval-after-load 'feature-name
;;       (message "eval-after-load1")
;;       (message "eval-after-load2")
;;       (message "eval-aftepr-others-after-load1")
;;       (message "eval-after-others-after-load2")))
;;
;;
;; =:eval-after-others-after-load= exists because similar reason to =:eval-after-others=.
;; Some action should be evaluated after all action added by other filters.
;; Because of same reasons as =:eval-before-all=, *it should NOT be used
;; by filters*.
;;
;;; Use default `mic'
;; `mic' is minimal for use.  `mic-core' is minimum core, but it is not enough to use as it is.
;; In addition to keywords allowed by [[mic-core][`mic-core']], it allows some keyword arguments:
;; - =:autoload-interactive=
;; - =:autoload-noninteractive=
;; - =:custom=
;; - =:custom-after-load=
;; - =:declare-function=
;; - =:define-key=
;; - =:define-key-after-load=
;; - =:define-key-with-feature=
;; - =:defvar-noninitial=
;; - =:face=
;; - =:hook=
;; - =:package=
;;
;;;; =:autoload-interactive=, =:autoload-noninteractive=
;;
;; These are transformed to `autoload' sexps.  Each element is function to autoload.
;; Since `autoload' should be informed whether the function is `interactive' or not,
;; both =:autoload-interactive= and =:autoload-noninteractive= exist.
;;
;;
;;   (mic feature-name
;;     :autoload-interactive
;;     (interactive-func1
;;      interactive-func2)
;;     :autoload-noninteractive
;;     (noninteractive-func3
;;      noninteractive-func4))
;;
;;   ;; Expanded to:
;;   (mic-core feature-name :eval
;;     ((autoload
;;      (autoload
;;      (autoload
;;      (autoload
;;     :eval-after-load nil)
;;
;;   ;; Expanded to:
;;   (prog1 'feature-name
;;     (autoload
;;     (autoload
;;     (autoload
;;     (autoload
;;
;;
;;;; =:custom=, =:custom-after-load=
;; These are transformed to `customize-set-variable' sexps.
;; Each element is =(VARIABLE . VALUE)=.
;; Each `VARIABLE' is set to `VALUE'.
;; Sexp from =:custom= argument are evaluated when the `mic' sexp is evaluated,
;; while sexp from =:custom-after-load= argument are evaluated after the feature is loaded.
;; =:custom-after-load= is used when you want to use initial value of customized variable
;; or function defined in the feature.
;;
;;
;;   (mic feature-name
;;     :custom
;;     ((variable1 . 1)
;;      ;; VALUE is evaluated
;;      (variable2 . (+ 1 1)))
;;     :custom-after-load
;;     ;; You can use the initial value of `variable3'
;;     ((variable3 . (+ variable3 1))
;;      ;; You can use function defined in the feature (for this example `feature-name')
;;      (variable2 . (function-defined-in-feature-name))))
;;
;;   ;; Expanded to:
;;   (mic-core feature-name
;;     :eval
;;     ((customize-set-variable 'variable1 1)
;;      (customize-set-variable 'variable2
;;                              (+ 1 1)))
;;     :eval-after-load
;;     ((customize-set-variable 'variable3
;;                              (+ variable3 1))
;;      (customize-set-variable 'variable2
;;                              (function-defined-in-feature-name))))
;;
;;   ;; Expanded to:
;;   (prog1 'feature-name
;;     (with-eval-after-load 'feature-name
;;       ;; `variable3' is already defined.
;;       (customize-set-variable 'variable3
;;                                (+ variable3 1))
;;       ;; `function-defined-in-feature-name' is already defined.
;;       (customize-set-variable 'variable2
;;                               (function-defined-in-feature-name)))
;;     (customize-set-variable 'variable1 1)
;;     (customize-set-variable 'variable2
;;                             (+ 1 1)))
;;
;;
;;;; `declare-function', `defvar-noninitial'
;; These arguments declare functions and variables.
;; Each element of `declare-function' / `defvar-noninitial' is symbol as function/variable.
;; They exist in order to suppress warning of undefined functions/variables.
;;
;;
;;   (mic feature-name
;;     :declare-function
;;     (function1
;;      function2)
;;     :defvar-noninitial
;;     (variable1
;;      variable2))
;;
;;   ;; Expanded to:
;;   (mic-core feature-name
;;     :eval
;;     ((declare-function function1 "ext:feature-name")
;;      (declare-function function2 "ext:feature-name")
;;      (defvar variable1)
;;      (defvar variable2))
;;     :eval-after-load nil)
;;
;;   ;; Expanded to:
;;   (prog1 'feature-name
;;     ;; They declare that the functions `function1' and `function2' is defined in
;;     ;; the feature `feature-name'.
;;     (declare-function function1 "ext:feature-name")
;;     (declare-function function2 "ext:feature-name")
;;     ;; They declare that the variables `variable1' and `variable2' will be defined.
;;     ;; `defvar' without initial value declares symbol as variable.
;;     (defvar variable1)
;;     (defvar variable2))
;;
;;
;;;; =:define-key=, =:define-key-after-load=, =:define-key-with-feature=
;; These arguments is transformed to `define-key' sexps.
;; On =:define-key= or =:define-key-after-load=, each element of the argument is
;; =(KEYMAP (KEYS . COMMAND)...)=.  `KEYMAP' is keymap.  `KEYS' is passed to `kbd'.
;; `COMMAND' is interactive function.
;;
;; On =:define-key-with-feature=, each element is =(FEATURE (KEYMAP (KEYS . COMMAND)...))=.
;; `FEATURE' is feature, and the `define-key' sexp is evaluated after loading the `FEATURE'.
;; This exists in order to define `COMMAND' in the feature with `KEYS' to `KEYMAP' defined in `FEATURE'.
;; Use it to make sure that `KEYMAP' is defined.
;;
;;
;;   (mic feature-name
;;     :define-key
;;     ;; (KEYMAP (KEYS . COMMAND)...)
;;     ((global-map
;;       ;;
;;       ("M-l" .
;;      (prog-mode-map
;;       ;;
;;       ("M-a" .
;;
;;     :define-key-after-load
;;     ;; When `feature-name-mode-map' is defined in `feature-name',
;;     ;; use `:define-key-after-load'.
;;     ((feature-name-mode-map
;;       ("M-r" .
;;       ("M-c" .
;;
;;
;;     ;; When `other-feature-mode-map' is defined in `other-feature', which is not `feature-name',
;;     ;; use `:define-key-with-feature'.
;;     :define-key-with-feature
;;     ((other-feature
;;       (other-feature-mode-map
;;        ("M-q" .
;;
;;   ;; Expanded to:
;;   (mic-core feature-name
;;     :eval
;;     ((define-key global-map (kbd "M-l")
;;      (define-key prog-mode-map (kbd "M-a")
;;      (with-eval-after-load 'other-feature
;;        (define-key other-feature-mode-map (kbd "M-q")
;;     :eval-after-load
;;     ((define-key feature-name-mode-map (kbd "M-r")
;;      (define-key feature-name-mode-map (kbd "M-c")
;;
;;   ;; Expanded to:
;;   (prog1 'feature-name
;;     (with-eval-after-load 'feature-name
;;       ;; `:define-key-after-load'
;;       (define-key feature-name-mode-map (kbd "M-r")
;;       (define-key feature-name-mode-map (kbd "M-c")
;;     ;; `:define-key'
;;     (define-key global-map (kbd "M-l")
;;     (define-key prog-mode-map (kbd "M-a")
;;     ;; `:define-key-with-feature'
;;     (with-eval-after-load 'other-feature
;;       (define-key other-feature-mode-map (kbd "M-q")
;;
;;
;;;; =:face=
;; This is transformed to `custom-set-faces' sexp.
;; Each element is =(FACE-SYMBOL . FACE-DEFINITION)=.
;;
;;
;;   (mic feature-name
;;     :face
;;     ((face-1
;;       . ((t (:foreground "red" :height 10.0))))
;;      (face-2
;;       . ((t (:background "
;;
;;   ;; Expanded to:
;;   (mic-core feature-name
;;     :eval
;;     ((custom-set-faces
;;       '(face-1
;;         ((t (:foreground "red" :height 10.0))))
;;       '(face-2
;;         ((t (:background "
;;     :eval-after-load nil)
;;
;;   ;; Expanded to:
;;   (prog1 'feature-name
;;     (custom-set-faces
;;      '(face-1
;;        ((t (:foreground "red" :height 10.0))))
;;      '(face-2
;;        ((t (:background "
;;
;;
;;;; =:hook=
;; This is transformed to `add-hook' sexp.
;; Each element is =(HOOK . FUNCTION)=.
;;
;;
;;   (mic feature-name
;;     :hook
;;     ;;
;;     ((hook1 .
;;      (hook2 .
;;      ;; `lambda' is allowed (but not recommended)
;;      (hook3 . (lambda (arg) 1))))
;;
;;   ;; Expanded to:
;;   (mic-core feature-name
;;     :eval
;;     ((add-hook 'hook1
;;      (add-hook 'hook2
;;      (add-hook 'hook3 (lambda (arg) 1)))
;;     :eval-after-load nil)
;;
;;   ;; Expanded to:
;;   (prog1 'feature-name
;;     (add-hook 'hook1
;;     (add-hook 'hook2
;;     (add-hook 'hook3 (lambda (arg) 1)))
;;
;;
;;;; =:package=
;; This is transformed to `package-install' sexps.
;; Each arguments are `PKG' used by `package-install'.
;;
;; The expandation result is complicated, because it is annoying to fetch package archives many times.
;;
;;
;;
;;   (mic feature-name
;;     :package
;;     (package-name1
;;      package-name2))
;;
;;   ;; Expanded to:
;;   (mic-core feature-name
;;     :eval
;;     ;; When package is not installed
;;     ((unless (package-installed-p 'package-name1)
;;        ;; Ensure package is exists in archive
;;        (when (assq 'package-name1 package-archive-contents)
;;          (ignore-errors
;;            (package-install 'package-name1)))
;;        (unless (package-installed-p 'package-name1)
;;          ;; Refresh (fetch) new archive
;;          (package-refresh-contents)
;;          (condition-case _
;;              (package-install 'package-name1)
;;            (error
;;             (warn "Package %s is not found" 'package-name1)))))
;;
;;      (unless (package-installed-p 'package-name2)
;;        (when (assq 'package-name2 package-archive-contents)
;;          (ignore-errors
;;            (package-install 'package-name2)))
;;        (unless (package-installed-p 'package-name2)
;;          (package-refresh-contents)
;;          (condition-case _
;;              (package-install 'package-name2)
;;            (error
;;             (warn "Package %s is not found" 'package-name2))))))
;;     :eval-after-load nil)
;;
;;   ;; Expand to:
;;   (prog1 'feature-name
;;     (unless (package-installed-p 'package-name1)
;;       (when (assq 'package-name1 package-archive-contents)
;;         (ignore-errors
;;           (package-install 'package-name1)))
;;       (unless (package-installed-p 'package-name1)
;;         (package-refresh-contents)
;;         (condition-case _
;;             (package-install 'package-name1)
;;           (error
;;            (warn "Package %s is not found" 'package-name1)))))
;;     (unless (package-installed-p 'package-name2)
;;       (when (assq 'package-name2 package-archive-contents)
;;         (ignore-errors
;;           (package-install 'package-name2)))
;;       (unless (package-installed-p 'package-name2)
;;         (package-refresh-contents)
;;         (condition-case _
;;             (package-install 'package-name2)
;;           (error
;;            (warn "Package %s is not found" 'package-name2))))))
;;
;;
;;; Define your own `mic'
;; PROPERTIES:
;; CUSTOM_ID: define-your-mic
;; END:
;; You do not like `mic' behavior? It is OK.  You can define your own `mic'!
;; There are some ways to define it:
;; - Use `mic-defmic'
;; - Use `defmacro'
;;
;;;; Define your own `mic' with `mic-defmic'
;; If you would like to add keywords, or to make some keywords more simple,
;; you can define `filter' and apply it to `mic' (or `mic-core', and another `mic', any parent is allowed).
;;
;;;;; Define filter
;; The filter recieves one argument, `PLIST' (plist, property list), and returns `RETURNED-PLIST'.
;; It filters or transforms it into returned plist.
;; It is better to divide filters by every keyword, because of reusability.
;;
;; 1. Each filter recieves 1 argument `PLIST', which is plist (property list).
;; 2. Each filter returns `RETURNED-PLIST', which is plist.
;; 3. `PLIST' is given by user or filter before.
;; 4. `PLIST' have feature name =:name= property.
;; 5. `RETURNED-PLIST' is passed to next filter or parent `mic' (`mic', `mic-core', or another).
;; 6. `RETURNED-PLIST' should have same value of =:name= property.
;; 7. The property only used by your filter should be removed in `RETURNED-PLIST'.
;;
;; Here is example:
;;
;;   (defun my-filter-global-set-key-without-quote (plist)
;;     (let ((alist
;;            ;; Get value from your own keyword
;;            (plist-get plist :bind))
;;           sexps)
;;       (setq sexps
;;             ;; Transform each element
;;             (mapcar
;;              (lambda (arg)
;;                (let ((keys (car arg))
;;                      (command (cdr arg)))
;;                  `(global-set-key (kbd ,keys)
;;              alist))
;;       ;; Put sexps to `:eval' arguments
;;       (mic-plist-put-append plist :eval sexps)
;;       ;; Don't forget to delete your own keyword!
;;       ;; When forget it, parent recieves it and may cause unexpected result.
;;       (mic-plist-delete plist :bind)
;;       plist))
;;
;;
;;;;; Define `mic' with the filter and `mic-defmic'
;; `mic-defmic' recieves arguments: `NAME', `PANRENT', optional `DOCSTRING', keyword argument `FILTERS'.
;; `NAME' is your new `mic' macro name.  `PARENT' is parent `mic', which recieves `RETURNED-PLIST' at last.
;; `FILTERS' is list of your filters.  When your `mic' recieves plist, the plist is filtered by all of your `FILTERS' in order,
;; then the plist is passed to `PARENT'.
;;
;; Here is example:
;;
;;   ;; Define `mymic'
;;   (mic-defmic mymic
;;     ;; Parent is here.  You can also use `mic-core'.
;;     mic
;;     :filters
;;     '(my-filter-global-set-key-without-quote
;;       ;; You can add other filters below
;;       ))
;;
;;   ;; Then you can use `mymic' like:
;;   (mymic simple
;;     :bind
;;     (("C-d" . delete-forward-char)
;;      ("C-x l" . toggle-truncate-lines))
;;     ;; Of course parent keywords are accepted.
;;     :custom
;;     ((kill-whole-line . t)
;;      (set-mark-command-repeat-pop . t)
;;      (mark-ring-max . 50)))
;;
;;   ;; Expanded to:
;;   (mic simple
;;     :custom
;;     ((kill-whole-line . t)
;;      (set-mark-command-repeat-pop . t)
;;      (mark-ring-max . 50))
;;     :eval
;;     ((global-set-key (kbd "C-d")
;;      (global-set-key (kbd "C-x l")
;;
;;
;; When you would like to use `mic-core' as `PARENT', `mic-filter-core-validate' is useful to validate plist.
;;;Please put it tail of `FILTERS' if you use it.*
;;
;;;; Define your own `mic' with `defmacro'
;; When you read here, you should know `defmacro'.
;; You can do anything with `defmacro'.  `mic-defmic' is easy way to define your `mic',
;; but may be not enough for you, because of restriction.  Then *I RECOMMEND to use `defmacro'*.
;; I am looking forward to seeing your `mic' defined by `defmacro'!
;;
;;; Alternative
;; PROPERTIES:
;; CUSTOM_ID: alternative
;; END:
;; There is some alternative:
;; - [[https://github.com/jwiegley/use-package][`use-package']]
;; - [[https://github.com/conao3/leaf.el][`leaf']]
;;
;; They are more easy to use, but sometimes have less expressive ability.
;; `mic' is more simple and has more expressive ability, but sometimes more redundant.
;; It is just your preference.
;;
;; In addition, they are customizable, while `mic' is not customizable, but re-definable.
;; You can define your own `mic' according to your preference, with `mic' help.
;;
;;; Contribute
;; When you think you would like to share your filter or your own `mic', use GitHub Discussion.
;; Of course your `mic' defined by `defmacro'.  Any issue is welcome.
;;
;;; License
;;   This package is licensed by GPLv3. See [[file:LICENSE][LICENSE]].

;; Minimal configuration manager

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'mic-deffilter)
(require 'mic-utils)

(defgroup mic ()
  "Minimal configuration manager."
  :group 'convenience
  :prefix "mic-"
  :link '(url-link "https://github.com/ROCKTAKEY/mic"))

;;;###autoload
(defmacro mic-apply-filter (plist name &rest filters)
  "Apply FILTERS to PLIST.
NAME is temporarily added to PLIST on :name property."
  (declare (indent defun))
  `(progn
     (mic-plist-put ,plist :name ,name)
     (setq ,plist (thread-last
                    ,plist
                    ,@filters))
     (mic-plist-delete ,plist :name)))

;;;###autoload
(defmacro mic-defmic (name parent docstring &rest plist)
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
  (mic-plist-put-append plist :eval
                         (mic-make-sexp-autoload-interactive
                          (plist-get plist :name)
                          (plist-get plist :autoload-interactive)))
  (mic-plist-delete plist :autoload-interactive)
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
  (mic-plist-put-append plist :eval
                         (mic-make-sexp-autoload-noninteractive
                          (plist-get plist :name)
                          (plist-get plist :autoload-noninteractive)))
  (mic-plist-delete plist :autoload-noninteractive)
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
  (mic-plist-put-append plist :eval
                         (mic-make-sexp-custom
                          (plist-get plist :custom)))
  (mic-plist-delete plist :custom)
  plist)

(defun mic-filter-custom-after-load (plist)
  "Append sexp from value of :custom-afer-load to value of :eval-after-load.
PLIST should be property list.
Sexp is generated by `mic-make-sexp-custom'."
  (mic-plist-put-append plist :eval-after-load
                         (mic-make-sexp-custom
                          (plist-get plist :custom-after-load)))
  (mic-plist-delete plist :custom-after-load)
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
  (mic-plist-put-append plist :eval
                         (mic-make-sexp-declare-function
                          (plist-get plist :name)
                          (plist-get plist :declare-function)))
  (mic-plist-delete plist :declare-function)
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
  (mic-plist-put-append plist :eval
                         (mic-make-sexp-define-key
                          (plist-get plist :define-key)))
  (mic-plist-delete plist :define-key)
  plist)

(defun mic-filter-define-key-after-load (plist)
  "Append sexp from value of :define-key-after-load to value of :eval-after-load.
PLIST should be property list.
Sexp is generated by `mic-make-sexp-define-key-after-load'."
  (mic-plist-put-append plist :eval-after-load
                         (mic-make-sexp-define-key
                          (plist-get plist :define-key-after-load)))
  (mic-plist-delete plist :define-key-after-load)
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
  (mic-plist-put-append plist :eval
                         (mic-make-sexp-define-key-with-feature
                          (plist-get plist :define-key-with-feature)))
  (mic-plist-delete plist :define-key-with-feature)
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
  (mic-plist-put-append plist :eval
                         (mic-make-sexp-defvar-noninitial
                          (plist-get plist :defvar-noninitial)))
  (mic-plist-delete plist :defvar-noninitial)
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
  (mic-plist-put-append plist :eval
                         (mic-make-sexp-face
                          (plist-get plist :face)))
  (mic-plist-delete plist :face)
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
  (mic-plist-put-append plist :eval
                         (mic-make-sexp-hook
                          (plist-get plist :hook)))
  (mic-plist-delete plist :hook)
  plist)

(defsubst mic-make-sexp-package (list)
  "Create `package-install' sexp from LIST.
Each element is package symbol."
  (mapcar
   (lambda (arg)
     `(unless (package-installed-p ',arg)
        (when (assq ',arg package-archive-contents)
          (ignore-errors (package-install ',arg)))
        (unless (package-installed-p ',arg)
          (package-refresh-contents)
          (condition-case _
              (package-install ',arg)
            (error
             (warn "Package %s is not found" ',arg))))))
   list))

(defun mic-filter-package (plist)
  "Append sexp from value of :package to value of :eval on PLIST.
Sexp is generated by `mic-make-sexp-package'."
  (mic-plist-put-append plist :eval-installation
                         (mic-make-sexp-package
                          (plist-get plist :package)))
  (mic-plist-delete plist :package)
  plist)

;;;###autoload (autoload 'mic-filter-core-validate "mic")
(mic-deffilter-validate mic-filter-core-validate
  :eval
  :eval-after-load
  :eval-after-others
  :eval-after-others-after-load
  :eval-before-all
  :eval-installation)



;;;###autoload
(cl-defmacro mic-core (name &key
                            eval
                            eval-after-load
                            eval-after-others
                            eval-after-others-after-load
                            eval-before-all
                            eval-installation)
  "Configuration package named NAME.
It evaluates each element of EVAL-BEFORE-ALL, EVAL, EVAL-AFTER-OTHERS in order.
In addition, It will evaluate each element of EVAL-AFTER-LOAD and
 EVAL-AFTER-OTHERS-AFTER-LOAD after load of package NAME."
  (declare (indent defun))
  `(prog1 ',name
     ,@eval-before-all
     ,@eval-installation
     ,@(and (or eval-after-load
                eval-after-others-after-load)
            (list
             (append
              (list 'with-eval-after-load `',name)
              eval-after-load
              eval-after-others-after-load)))
     ,@eval
     ,@eval-after-others))

;;;###autoload (autoload 'mic "mic" nil nil 'macro)
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
    mic-filter-package
    mic-filter-core-validate))

(provide 'mic)
;;; mic.el ends here
