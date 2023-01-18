;;; mic.el --- Minimal and combinable configuration manager  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: convenience

;; Version: 0.34.4
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

;; Table of Contents
;; _________________

;; 1. mic: Minimal and combinable configuration manager for Emacs
;; 2. How to Use?
;; 3. Use `mic-core', minimum one
;; .. 1. `:eval', `:eval-after-others', `:eval-before-all'
;; .. 2. `:eval-after-load', `:eval-after-others-after-load'
;; .. 3. `:eval-installation'
;; 4. Use default `mic'
;; .. 1. `:autoload-interactive', `:autoload-noninteractive'
;; .. 2. `:auto-mode'
;; .. 3. `:custom', `:custom-after-load'
;; .. 4. `declare-function', `defvar-noninitial'
;; .. 5. `:define-key', `:define-key-after-load', `:define-key-with-feature'
;; .. 6. `:face'
;; .. 7. `:hook'
;; .. 8. `:package'
;; .. 9. `:require'
;; .. 10. `:require-after'
;; 5. Define your own `mic'
;; .. 1. Define your own `mic' with `mic-defmic'
;; ..... 1. What is a filter?
;; ..... 2. Pre-defined filters
;; ..... 3. Helper for defining a filter
;; ..... 4. Define `mic' with `mic-defmic'
;; .. 2. Define your own `mic' with `defmacro'
;; 6. Alternative
;; 7. Contribute
;; 8. License


;; [https://img.shields.io/github/tag/ROCKTAKEY/mic.svg?style=flat-square]
;; [https://img.shields.io/github/license/ROCKTAKEY/mic.svg?style=flat-square]
;; [https://img.shields.io/codecov/c/github/ROCKTAKEY/mic.svg?style=flat-square]
;; [https://img.shields.io/github/actions/workflow/status/ROCKTAKEY/mic/test.yml.svg?branch=master&style=flat-square]


;; [https://img.shields.io/github/tag/ROCKTAKEY/mic.svg?style=flat-square]
;; <https://github.com/ROCKTAKEY/mic>

;; [https://img.shields.io/github/license/ROCKTAKEY/mic.svg?style=flat-square]
;; <file:LICENSE>

;; [https://img.shields.io/codecov/c/github/ROCKTAKEY/mic.svg?style=flat-square]
;; <https://codecov.io/gh/ROCKTAKEY/mic?branch=master>

;; [https://img.shields.io/github/actions/workflow/status/ROCKTAKEY/mic/test.yml.svg?branch=master&style=flat-square]
;; <https://github.com/ROCKTAKEY/mic/actions>


;; 1 mic: Minimal and combinable configuration manager for Emacs
;; =============================================================

;;         `mic' is uncustomizable.  Define your own `mic'.

;;   `mic' is minimal and combinable configuration manager for Emacs.  This
;;   package is yet another `use-package' and `leaf', but is also used with
;;   them (See [Alternative]).  `mic' is minimal, so if you would like to
;;   write complex configuration, `mic' is a little redundant.  However,
;;   there is no problem.  `mic' is combinable, in the other words, thought
;;   to be used as core to define your own, and more convenient `mic'.
;;   There are some functions to define your own `mic'.  See [Define your
;;   own mic].


;; [Alternative] See section 6

;; [Define your own mic] See section 5


;; 2 How to Use?
;; =============

;;   For Emacs Lisp beginners, original `mic' macro is useful to configure
;;   your `init.el'.
;;   ,----
;;   | (mic lsp-mode
;;   |   ;; These are transformed to `define-key' sexp.
;;   |   ;; Each argument is `(KEYMAP (KEYS . COMMAND)...)'.
;;   |   ;; KEYS is passed to `kbd'.
;;   |   :define-key
;;   |   ((global-map
;;   |     ("M-l" . #'lsp)))
;;   |
;;   |   ;; These are same as `:define-key' argument,
;;   |   ;; but evaluated after loading the feature (`lsp-mode' for this example).
;;   |   ;; This is needed because `lsp-mode-map' is unavailable before `lsp'
;;   |   ;; loading.
;;   |   :define-key-after-load
;;   |   ((lsp-mode-map
;;   |     ("M-r" . #'lsp-rename)
;;   |     ("M-c" . #'lsp-execute-code-action)))
;;   |
;;   |   ;; These are transformed to `with-eval-after-load' and `define-key' sexp.
;;   |   ;; Each argument is `(FEATURE (KEYMAP (KEYS . COMMAND)...))'.
;;   |   ;; `cdr' is same as `:define-key' arguments.  Each `define-key' sexp is
;;   |   ;; evaluated after FEATURE is loaded.
;;   |   ;; This is needed because `dired-mode-map' is unavailable before `dired'
;;   |   ;; loading.
;;   |   :define-key-with-feature
;;   |   ((`dired'
;;   |     (dired-mode-map
;;   |      ("M-q" . #'lsp-dired-mode))))
;;   |
;;   |   ;; These are transformed to `customize-set-variable' sexp.
;;   |   ;; Each argument is `(VARIABLE . VALUE)'.
;;   |   :custom
;;   |   ((lsp-sesstion-file . (expand-file-name "etc/.lsp-session-v1" user-emacs-directory))
;;   |    (lsp-log-io . t))
;;   |
;;   |   ;; These are transformed to `add-hook' sexp.
;;   |   ;; Each argument is `(HOOK . FUNCTION)'.
;;   |   :hook
;;   |   ((c-mode-hook . #'lsp)
;;   |    (c++-mode-hook . #'lsp)
;;   |    (tex-mode-hook . #'lsp)
;;   |    (latex-mode-hook . #'lsp)
;;   |    (bibtex-mode-hook . #'lsp)
;;   |    (rust-mode-hook . #'lsp))
;;   |
;;   |   ;; Each element is evaluated immediately when this `mic' sexp is evaluated.
;;   |   :eval
;;   |   ((message "This is evaluated when this `mic' sexp is evaluated.")
;;   |    (message "This is also evaluated."))
;;   |
;;   |   ;; Each element will be evaluated after the package (`lsp-mode' for this example) is loaded.
;;   |   :eval-after-load
;;   |   ((message "This is evaluated when `lsp-mode' is loaded."))
;;   |
;;   |   ;; Each element is evaluated immediately when this `mic' sexp is evaluated.
;;   |   ;; These are evaluated before `:eval' and `:eval-after-load' elements.
;;   |   ;; This is for such use as defining function to use `:custom' argument.
;;   |   :eval-before-all
;;   |   ((message "This is evaluated when this `mic' sexp is evaluated.")
;;   |    (message "These are evaluated before `:eval' and `:eval-after-load' sexp.")))
;;   |
;;   |
;;   | ;; `mic' sexp above is expanded to:
;;   | (prog1 'lsp-mode
;;   |   ;; `:eval-before-all'
;;   |   (message "This is evaluated when this `mic' sexp is evaluated.")
;;   |   (message "These are evaluated before `:eval' and `:eval-after-load' sexp.")
;;   |
;;   |   ;; `:eval-after-load'
;;   |   (with-eval-after-load 'lsp-mode
;;   |     (message "This is evaluated when `lsp-mode' is loaded.")
;;   |     ;; `:define-key-after-load'
;;   |     (define-key lsp-mode-map
;;   |       (kbd "M-r")
;;   |       (function lsp-rename))
;;   |     (define-key lsp-mode-map
;;   |       (kbd "M-c")
;;   |       (function lsp-execute-code-action)))
;;   |
;;   |   ;; `:eval'
;;   |   (message "This is evaluated when this `mic' sexp is evaluated.")
;;   |   (message "This is also evaluated.")
;;   |
;;   |   ;; `:custom'
;;   |   (customize-set-variable 'lsp-sesstion-file
;;   |                            (expand-file-name "etc/.lsp-session-v1" user-emacs-directory))
;;   |   (customize-set-variable 'lsp-log-io t)
;;   |
;;   |   ;; `:define-key'
;;   |   (define-key global-map (kbd "M-l") #'lsp)
;;   |
;;   |   ;; `:define-key-with-feature'
;;   |   (with-eval-after-load '`dired'
;;   |     (define-key dired-mode-map (kbd "M-q") #'lsp-dired-mode))
;;   |
;;   |   ;; `:hook'
;;   |   (add-hook 'c-mode-hook #'lsp)
;;   |   (add-hook 'c++-mode-hook #'lsp)
;;   |   (add-hook 'tex-mode-hook #'lsp)
;;   |   (add-hook 'latex-mode-hook #'lsp)
;;   |   (add-hook 'bibtex-mode-hook #'lsp)
;;   |   (add-hook 'rust-mode-hook #'lsp))
;;   `----

;;   For Emacs Lisp expert, original `mic' is a little unsatisfactory or
;;   redundant.  `mic' is not customizable, but you can define your own
;;   `mic' easily.
;;   1. Determine parent.  You can use as parent `mic', `mic-core', which is
;;      simpler `mic'.  `mic-core' recieves only keywords start from
;;      `:eval', such as `:eval', `eval-after-load'.
;;   2. Define filter functions.  Each one recieves plist (property list)
;;      and returns plist.  returned plist is passed to parent (such as
;;      `mic', `mic-core') or next filter.  Note that filter function can
;;      get feature name as value of property `:name'.  Of course, you can
;;      use pre-defined filters.  `mic' is defined by some filters from the
;;      parent `mic-core'.
;;   3. Define your own mic by `mic-defmic'.  It recieves `NAME', optional
;;      `DOCSTRING', and keyword argument `FILTERS'.  `NAME' is name of your
;;      own `mic'.  `DOCSTRING' is the document string of yours.  `FILTERS'
;;      are list of filter.  As explained, filter recieves plist and
;;      returns plist.  It filter plist to get desired behavior.

;;   ,----
;;   | (defun my-filter-global-set-key-without-quote (plist)
;;   |   (let ((alist
;;   |          ;; Get value from your own keyword
;;   |          (plist-get plist :bind))
;;   |         sexps)
;;   |     (setq sexps
;;   |           ;; Transform each element
;;   |           (mapcar
;;   |            (lambda (arg)
;;   |              (let ((keys (car arg))
;;   |                    (command (cdr arg)))
;;   |                `(global-set-key (kbd ,keys) #',command)))
;;   |            alist))
;;   |     ;; Put sexps to `:eval' arguments
;;   |     (mic-plist-put-append plist :eval sexps)
;;   |     ;; Don't forget to delete your own keyword!
;;   |     ;; When forget it, parent recieves it and may cause unexpected result.
;;   |     (mic-plist-delete plist :bind)
;;   |     plist))
;;   |
;;   | (mic-defmic mymic
;;   |   ;; Parent is here.  You can also use `mic-core'.
;;   |   mic
;;   |   :filters
;;   |   '(my-filter-global-set-key-without-quote
;;   |     ;; You can add other filters below
;;   |     ))
;;   |
;;   | ;; Then you can use `mymic' like:
;;   | (mymic simple
;;   |   :bind
;;   |   (("C-d" . delete-forward-char)
;;   |    ("C-x l" . toggle-truncate-lines))
;;   |   ;; Of course parent keywords are accepted.
;;   |   :custom
;;   |   ((kill-whole-line . t)
;;   |    (set-mark-command-repeat-pop . t)
;;   |    (mark-ring-max . 50)))
;;   |
;;   | ;; `mymic' sexp is expanded to:
;;   | (mic simple
;;   |   :custom
;;   |   ((kill-whole-line . t)
;;   |    (set-mark-command-repeat-pop . t)
;;   |    (mark-ring-max . 50))
;;   |   :eval
;;   |   ((global-set-key (kbd "C-d") #'delete-forward-char)
;;   |    (global-set-key (kbd "C-x l") #'toggle-truncate-lines)))
;;   |
;;   | ;; Expanded to:
;;   | (mic-core simple
;;   |   :eval
;;   |   ((global-set-key (kbd "C-d") #'delete-forward-char)
;;   |    (global-set-key (kbd "C-x l") #'toggle-truncate-lines)
;;   |    (customize-set-variable 'kill-whole-line t)
;;   |    (customize-set-variable 'set-mark-command-repeat-pop t)
;;   |    (customize-set-variable 'mark-ring-max 50))
;;   |   :eval-after-load nil)
;;   |
;;   | ;; Expanded to:
;;   | (prog1 'simple
;;   |   (global-set-key  (kbd "C-d") #'delete-forward-char)
;;   |   (global-set-key (kbd "C-x l") #'toggle-truncate-lines)
;;   |   (customize-set-variable 'kill-whole-line t)
;;   |   (customize-set-variable 'set-mark-command-repeat-pop t)
;;   |   (customize-set-variable 'mark-ring-max 50))
;;   `----


;; 3 Use `mic-core', minimum one
;; =============================

;;   `mic-core' is minimum.  It can recieves only several keywords:
;;   - `:eval'
;;   - `:eval-after-load'
;;   - `:eval-after-others'
;;   - `:eval-after-others-after-load'
;;   - `:eval-before-all'
;;   - `:eval-installation'

;;   Each element of `:eval' arguments are evaluated.  Time to evaluate is
;;   different.


;; 3.1 `:eval', `:eval-after-others', `:eval-before-all'
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   Each element of these arguments are evaluated when the `mic' sexp is
;;   evaluated.  The order is:
;;   - `:eval-before-all'
;;   - (`with-eval-after-load' sexp, explained on [`eval-after-load'
;;     keyword section], is evaluated)
;;   - `:eval'
;;   - `:eval-after-others'

;;   ,----
;;   | (mic-core feature-name
;;   |   :eval
;;   |   ((message "eval1")
;;   |    (message "eval2"))
;;   |   :eval-after-others
;;   |   ((message "eval-after-others1")
;;   |    (message "eval-after-others2"))
;;   |   :eval-before-all
;;   |   ((message "eval-before-all1")
;;   |    (message "eval-before-all2"))
;;   |   :eval-after-load
;;   |   ((message "eval-after-load1")
;;   |    (message "eval-after-load2")))
;;   |
;;   | ;; Expanded to:
;;   | (prog1 'feature-name
;;   |   (message "eval-before-all1")
;;   |   (message "eval-before-all2")
;;   |   (with-eval-after-load 'feature-name
;;   |     (message "eval-after-load1")
;;   |     (message "eval-after-load2"))
;;   |   (message "eval1")
;;   |   (message "eval2")
;;   |   (message "eval-after-others1")
;;   |   (message "eval-after-others2"))
;;   `----

;;   `:eval-before-all' exists because a filter function appends sexp to
;;   `:eval' argument.  When some action should be evaluated before all
;;   action added by other filters, you can put it to `:eval-before-all'
;;   argument.  *Note that it should NOT be used by filters.* Any filter
;;   should not use this.  If it is used by filters, users cannot make their
;;   sexp to be evaluate before filter sexps.

;;   `:eval-after-others' exists because similar reason to
;;   `:eval-before-all'.  Some action should be evaluated after all action
;;   added by other filters.  Because of same reasons as
;;   `:eval-before-all', *it should NOT be used by filters*.


;; [`eval-after-load' keyword section] See section 3.2


;; 3.2 `:eval-after-load', `:eval-after-others-after-load'
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   Each element of these arguments are evaluated after the package is
;;   loaded.  The evaluated order is:
;;   - `:eval-after-load'
;;   - `:eval-after-others-after-load'

;;   ,----
;;   | (mic-core feature-name
;;   |   :eval-after-load
;;   |   ((message "eval-after-load1")
;;   |    (message "eval-after-load2"))
;;   |   :eval-after-others-after-load
;;   |   ((message "eval-after-others-after-load1")
;;   |    (message "eval-after-others-after-load2")))
;;   |
;;   | ;; Expanded to:
;;   | (prog1 'feature-name
;;   |   (with-eval-after-load 'feature-name
;;   |     (message "eval-after-load1")
;;   |     (message "eval-after-load2")
;;   |     (message "eval-aftepr-others-after-load1")
;;   |     (message "eval-after-others-after-load2")))
;;   `----

;;   `:eval-after-others-after-load' exists because similar reason to
;;   `:eval-after-others'.  Some action should be evaluated after all
;;   action added by other filters.  Because of same reasons as
;;   `:eval-before-all', *it should NOT be used by filters*.


;; 3.3 `:eval-installation'
;; ~~~~~~~~~~~~~~~~~~~~~~~~

;;   Each element of this argument is evaluated before evaluation of other
;;   `:eval*' argument except `:eval-before-all'.  This exists because sexp
;;   to install the package is evaluated before sexp which uses package
;;   features.

;;   ,----
;;   | (mic-core feature-name
;;   |   :eval-before-all
;;   |   ((message "before all2")
;;   |    (message "before all1"))
;;   |   :eval-installation
;;   |   ((message "install1")
;;   |    (message "install2"))
;;   |   :eval-after-load
;;   |   ((message "eval-after-load1")
;;   |    (message "eval-after-load2"))
;;   |   :eval-after-others-after-load
;;   |   ((message "eval-after-others-after-load1")
;;   |    (message "eval-after-others-after-load2"))
;;   |   :eval
;;   |   ((message "eval1")
;;   |    (message "eval2")))
;;   |
;;   | ;; Expanded to:
;;   | (prog1 'feature-name
;;   |   (message "before all2")
;;   |   (message "before all1")
;;   |   (message "install1")
;;   |   (message "install2")
;;   |   (with-eval-after-load 'feature-name
;;   |     (message "eval-after-load1")
;;   |     (message "eval-after-load2")
;;   |     (message "eval-after-others-after-load1")
;;   |     (message "eval-after-others-after-load2"))
;;   |   (message "eval1")
;;   |   (message "eval2"))
;;   `----

;;   `:eval-after-others-after-load' exists because similar reason to
;;   `:eval-after-others'.  Some action should be evaluated after all
;;   action added by other filters.  Because of same reasons as
;;   `:eval-before-all', *it should NOT be used by filters*.


;; 4 Use default `mic'
;; ===================

;;   `mic' is minimal for use.  `mic-core' is minimum core, but it is not
;;   enough to use as it is.  In addition to keywords allowed by
;;   [`mic-core'], it allows some keyword arguments:
;;   - `:autoload-interactive'
;;   - `:autoload-noninteractive'
;;   - `:auto-mode'
;;   - `:custom'
;;   - `:custom-after-load'
;;   - `:declare-function'
;;   - `:define-key'
;;   - `:define-key-after-load'
;;   - `:define-key-with-feature'
;;   - `:defvar-noninitial'
;;   - `:face'
;;   - `:hook'
;;   - `:package'
;;   - `:require'
;;   - `:require-after'


;; [`mic-core'] See section 3

;; 4.1 `:autoload-interactive', `:autoload-noninteractive'
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   These are transformed to `autoload' sexps.  Each element is function to
;;   autoload.  Since `autoload' should be informed whether the function is
;;   `interactive' or not, both `:autoload-interactive' and
;;   `:autoload-noninteractive' exist.

;;   ,----
;;   | (mic feature-name
;;   |   :autoload-interactive
;;   |   (interactive-func1
;;   |    interactive-func2)
;;   |   :autoload-noninteractive
;;   |   (noninteractive-func3
;;   |    noninteractive-func4))
;;   |
;;   | ;; Expanded to:
;;   | (mic-core feature-name :eval
;;   |   ((autoload #'interactive-func1 "feature-name" nil t)
;;   |    (autoload #'interactive-func2 "feature-name" nil t)
;;   |    (autoload #'noninteractive-func3 "feature-name")
;;   |    (autoload #'noninteractive-func4 "feature-name"))
;;   |   :eval-after-load nil)
;;   |
;;   | ;; Expanded to:
;;   | (prog1 'feature-name
;;   |   (autoload #'interactive-func1 "feature-name" nil t)
;;   |   (autoload #'interactive-func2 "feature-name" nil t)
;;   |   (autoload #'noninteractive-func3 "feature-name")
;;   |   (autoload #'noninteractive-func4 "feature-name"))
;;   `----


;; 4.2 `:auto-mode'
;; ~~~~~~~~~~~~~~~~

;;   It is transformed to sexp like `(add-to-list 'auto-mode-alist ...)'.
;;   Each element of the value should be valid as an element of
;;   `auto-mode-alist'.

;;   ,----
;;   | (mic feature-name
;;   |   :auto-mode
;;   |   (("\\.html?\\'" . web-mode)
;;   |    ("\\.css\\'" . web-mode)))
;;   |
;;   | ;; Expanded to:
;;   | (mic-core feature-name :eval-installation
;;   |   ((add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;;   |    (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode)))
;;   |   :eval nil :eval-after-load nil)
;;   |
;;   | ;; Expanded to:
;;   | (prog1 'feature-name
;;   |   (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;;   |   (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode)))
;;   |

;;   `----


;; 4.3 `:custom', `:custom-after-load'
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   These are transformed to `customize-set-variable' sexps.  Each element
;;   is `(VARIABLE . VALUE)'.  Each `VARIABLE' is set to `VALUE'.  Sexp
;;   from `:custom' argument are evaluated when the `mic' sexp is
;;   evaluated, while sexp from `:custom-after-load' argument are evaluated
;;   after the feature is loaded.  `:custom-after-load' is used when you
;;   want to use initial value of customized variable or function defined
;;   in the feature.

;;   ,----
;;   | (mic feature-name
;;   |   :custom
;;   |   ((variable1 . 1)
;;   |    ;; VALUE is evaluated
;;   |    (variable2 . (+ 1 1)))
;;   |   :custom-after-load
;;   |   ;; You can use the initial value of `variable3'
;;   |   ((variable3 . (+ variable3 1))
;;   |    ;; You can use function defined in the feature (for this example `feature-name')
;;   |    (variable2 . (function-defined-in-feature-name))))
;;   |
;;   | ;; Expanded to:
;;   | (mic-core feature-name
;;   |   :eval
;;   |   ((customize-set-variable 'variable1 1)
;;   |    (customize-set-variable 'variable2
;;   |                            (+ 1 1)))
;;   |   :eval-after-load
;;   |   ((customize-set-variable 'variable3
;;   |                            (+ variable3 1))
;;   |    (customize-set-variable 'variable2
;;   |                            (function-defined-in-feature-name))))
;;   |
;;   | ;; Expanded to:
;;   | (prog1 'feature-name
;;   |   (with-eval-after-load 'feature-name
;;   |     ;; `variable3' is already defined.
;;   |     (customize-set-variable 'variable3
;;   |                              (+ variable3 1))
;;   |     ;; `function-defined-in-feature-name' is already defined.
;;   |     (customize-set-variable 'variable2
;;   |                             (function-defined-in-feature-name)))
;;   |   (customize-set-variable 'variable1 1)
;;   |   (customize-set-variable 'variable2
;;   |                           (+ 1 1)))
;;   `----


;; 4.4 `declare-function', `defvar-noninitial'
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   These arguments declare functions and variables.  Each element of
;;   `declare-function' / `defvar-noninitial' is symbol as
;;   function/variable.  They exist in order to suppress warning of
;;   undefined functions/variables.

;;   ,----
;;   | (mic feature-name
;;   |   :declare-function
;;   |   (function1
;;   |    function2)
;;   |   :defvar-noninitial
;;   |   (variable1
;;   |    variable2))
;;   |
;;   | ;; Expanded to:
;;   | (mic-core feature-name
;;   |   :eval
;;   |   ((declare-function function1 "ext:feature-name")
;;   |    (declare-function function2 "ext:feature-name")
;;   |    (defvar variable1)
;;   |    (defvar variable2))
;;   |   :eval-after-load nil)
;;   |
;;   | ;; Expanded to:
;;   | (prog1 'feature-name
;;   |   ;; They declare that the functions `function1' and `function2' is defined in
;;   |   ;; the feature `feature-name'.
;;   |   (declare-function function1 "ext:feature-name")
;;   |   (declare-function function2 "ext:feature-name")
;;   |   ;; They declare that the variables `variable1' and `variable2' will be defined.
;;   |   ;; `defvar' without initial value declares symbol as variable.
;;   |   (defvar variable1)
;;   |   (defvar variable2))
;;   `----


;; 4.5 `:define-key', `:define-key-after-load', `:define-key-with-feature'
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   These arguments is transformed to `define-key' sexps.  On
;;   `:define-key' or `:define-key-after-load', each element of the
;;   argument is `(KEYMAP (KEYS . COMMAND)...)'.  `KEYMAP' is keymap.  `KEYS'
;;   is passed to `kbd'.  `COMMAND' is interactive function.

;;   On `:define-key-with-feature', each element is `(FEATURE (KEYMAP (KEYS
;;   . COMMAND)...))'.  `FEATURE' is feature, and the `define-key' sexp is
;;   evaluated after loading the `FEATURE'.  This exists in order to define
;;   `COMMAND' in the feature with `KEYS' to `KEYMAP' defined in `FEATURE'.
;;   Use it to make sure that `KEYMAP' is defined.

;;   ,----
;;   | (mic feature-name
;;   |   :define-key
;;   |   ;; (KEYMAP (KEYS . COMMAND)...)
;;   |   ((global-map
;;   |     ;; #' is needed
;;   |     ("M-l" . #'feature-name-command1))
;;   |    (prog-mode-map
;;   |     ;; #' is needed
;;   |     ("M-a" . #'feature-name-comman2)))
;;   |
;;   |   :define-key-after-load
;;   |   ;; When `feature-name-mode-map' is defined in `feature-name',
;;   |   ;; use `:define-key-after-load'.
;;   |   ((feature-name-mode-map
;;   |     ("M-r" . #'feature-name-command3)
;;   |     ("M-c" . #'feature-name-command4)))
;;   |
;;   |
;;   |   ;; When `other-feature-mode-map' is defined in `other-feature', which is not `feature-name',
;;   |   ;; use `:define-key-with-feature'.
;;   |   :define-key-with-feature
;;   |   ((other-feature
;;   |     (other-feature-mode-map
;;   |      ("M-q" . #'feature-name-command5)))))
;;   |
;;   | ;; Expanded to:
;;   | (mic-core feature-name
;;   |   :eval
;;   |   ((define-key global-map (kbd "M-l") #'feature-name-command1)
;;   |    (define-key prog-mode-map (kbd "M-a") #'feature-name-comman2)
;;   |    (with-eval-after-load 'other-feature
;;   |      (define-key other-feature-mode-map (kbd "M-q") #'feature-name-command5)))
;;   |   :eval-after-load
;;   |   ((define-key feature-name-mode-map (kbd "M-r") #'feature-name-command3)
;;   |    (define-key feature-name-mode-map (kbd "M-c") #'feature-name-command4)))
;;   |
;;   | ;; Expanded to:
;;   | (prog1 'feature-name
;;   |   (with-eval-after-load 'feature-name
;;   |     ;; `:define-key-after-load'
;;   |     (define-key feature-name-mode-map (kbd "M-r") #'feature-name-command3)
;;   |     (define-key feature-name-mode-map (kbd "M-c") #'feature-name-command4))
;;   |   ;; `:define-key'
;;   |   (define-key global-map (kbd "M-l") #'feature-name-command1)
;;   |   (define-key prog-mode-map (kbd "M-a") #'feature-name-comman2)
;;   |   ;; `:define-key-with-feature'
;;   |   (with-eval-after-load 'other-feature
;;   |     (define-key other-feature-mode-map (kbd "M-q") #'feature-name-command5)))
;;   `----


;; 4.6 `:face'
;; ~~~~~~~~~~~

;;   This is transformed to `custom-set-faces' sexp.  Each element is
;;   `(FACE-SYMBOL . FACE-DEFINITION)'.

;;   ,----
;;   | (mic feature-name
;;   |   :face
;;   |   ((face-1
;;   |     . ((t (:foreground "red" :height 10.0))))
;;   |    (face-2
;;   |     . ((t (:background "#006000" :foreground "white" :bold t))))))
;;   |
;;   | ;; Expanded to:
;;   | (mic-core feature-name
;;   |   :eval
;;   |   ((custom-set-faces
;;   |     '(face-1
;;   |       ((t (:foreground "red" :height 10.0))))
;;   |     '(face-2
;;   |       ((t (:background "#006000" :foreground "white" :bold t))))))
;;   |   :eval-after-load nil)
;;   |
;;   | ;; Expanded to:
;;   | (prog1 'feature-name
;;   |   (custom-set-faces
;;   |    '(face-1
;;   |      ((t (:foreground "red" :height 10.0))))
;;   |    '(face-2
;;   |      ((t (:background "#006000" :foreground "white" :bold t))))))
;;   `----


;; 4.7 `:hook'
;; ~~~~~~~~~~~

;;   This is transformed to `add-hook' sexp.  Each element is `(HOOK
;;   . FUNCTION)'.

;;   ,----
;;   | (mic feature-name
;;   |   :hook
;;   |   ;; #' is needed
;;   |   ((hook1 . #'function1)
;;   |    (hook2 . #'function2)
;;   |    ;; `lambda' is allowed (but not recommended)
;;   |    (hook3 . (lambda (arg) 1))))
;;   |
;;   | ;; Expanded to:
;;   | (mic-core feature-name
;;   |   :eval
;;   |   ((add-hook 'hook1 #'function1)
;;   |    (add-hook 'hook2 #'function2)
;;   |    (add-hook 'hook3 (lambda (arg) 1)))
;;   |   :eval-after-load nil)
;;   |
;;   | ;; Expanded to:
;;   | (prog1 'feature-name
;;   |   (add-hook 'hook1 #'function1)
;;   |   (add-hook 'hook2 #'function2)
;;   |   (add-hook 'hook3 (lambda (arg) 1)))
;;   `----


;; 4.8 `:package'
;; ~~~~~~~~~~~~~~

;;   This is transformed to `package-install' sexps.  Each arguments are
;;   `PKG' used by `package-install'.

;;   The expandation result is complicated, because it is annoying to fetch
;;   package archives many times.


;;   ,----
;;   | (mic feature-name
;;   |   :package
;;   |   (package-name1
;;   |    package-name2))
;;   |
;;   | ;; Expanded to:
;;   | (mic-core feature-name
;;   |   :eval
;;   |   ;; When package is not installed
;;   |   ((unless (package-installed-p 'package-name1)
;;   |      ;; Ensure package is exists in archive
;;   |      (when (assq 'package-name1 package-archive-contents)
;;   |        (ignore-errors
;;   |          (package-install 'package-name1)))
;;   |      (unless (package-installed-p 'package-name1)
;;   |        ;; Refresh (fetch) new archive
;;   |        (package-refresh-contents)
;;   |        (condition-case _
;;   |            (package-install 'package-name1)
;;   |          (error
;;   |           (warn "Package %s is not found" 'package-name1)))))
;;   |
;;   |    (unless (package-installed-p 'package-name2)
;;   |      (when (assq 'package-name2 package-archive-contents)
;;   |        (ignore-errors
;;   |          (package-install 'package-name2)))
;;   |      (unless (package-installed-p 'package-name2)
;;   |        (package-refresh-contents)
;;   |        (condition-case _
;;   |            (package-install 'package-name2)
;;   |          (error
;;   |           (warn "Package %s is not found" 'package-name2))))))
;;   |   :eval-after-load nil)
;;   |
;;   | ;; Expand to:
;;   | (prog1 'feature-name
;;   |   (unless (package-installed-p 'package-name1)
;;   |     (when (assq 'package-name1 package-archive-contents)
;;   |       (ignore-errors
;;   |         (package-install 'package-name1)))
;;   |     (unless (package-installed-p 'package-name1)
;;   |       (package-refresh-contents)
;;   |       (condition-case _
;;   |           (package-install 'package-name1)
;;   |         (error
;;   |          (warn "Package %s is not found" 'package-name1)))))
;;   |   (unless (package-installed-p 'package-name2)
;;   |     (when (assq 'package-name2 package-archive-contents)
;;   |       (ignore-errors
;;   |         (package-install 'package-name2)))
;;   |     (unless (package-installed-p 'package-name2)
;;   |       (package-refresh-contents)
;;   |       (condition-case _
;;   |           (package-install 'package-name2)
;;   |         (error
;;   |          (warn "Package %s is not found" 'package-name2))))))
;;   `----


;; 4.9 `:require'
;; ~~~~~~~~~~~~~~

;;   This is transformed to `require' sexps.  Each element is feature
;;   symbol and required on `:eval'.

;;   ,----
;;   | (mic feature-name
;;   |   :require
;;   |   (feat1
;;   |    feat2))
;;   |
;;   | ;; Expand to:
;;   | (mic-core feature-name
;;   |   :eval-installation nil
;;   |   :eval
;;   |   ((require 'feat1)
;;   |    (require 'feat2))
;;   |   :eval-after-load nil)
;;   |
;;   | ;; Expand to:
;;   | (prog1 'feature-name
;;   |   (require 'feat1)
;;   |   (require 'feat2))
;;   `----


;; 4.10 `:require-after'
;; ~~~~~~~~~~~~~~~~~~~~~

;;   This is transformed to `require' sexps in `with-eval-after-load'
;;   section.  Each element is alist.  `car' of each element is feature
;;   symbol which is used as first argument of `with-eval-after-load'.
;;   `cdr' of each element is list of features required after the `car'.

;;   This is used when you should require package after another one but
;;   there is no functions to call so `autoload' cannot be used.

;;   ,----
;;   | (mic feature-name
;;   |   :require-after
;;   |   ((feat-after1
;;   |     . (feat1  feat2))
;;   |    (feat-after2
;;   |     feat3
;;   |     feat4)))
;;   |
;;   | ;; Expand to:
;;   | (mic-core feature-name
;;   |   :eval-installation nil
;;   |   :eval
;;   |   ((with-eval-after-load 'feat-after1
;;   |      (require 'feat1)
;;   |      (require 'feat2))
;;   |    (with-eval-after-load 'feat-after2
;;   |      (require 'feat3)
;;   |      (require 'feat4)))
;;   |   :eval-after-load nil)
;;   |
;;   | ;; Expand to:
;;   | (prog1 'feature-name
;;   |   (with-eval-after-load 'feat-after1
;;   |     (require 'feat1)
;;   |     (require 'feat2))
;;   |   (with-eval-after-load 'feat-after2
;;   |     (require 'feat3)
;;   |     (require 'feat4)))
;;   `----


;; 5 Define your own `mic'
;; =======================

;;   You do not like `mic' behavior? It is OK.  You can define your own
;;   `mic'!  There are some ways to define it:
;;   - Use `mic-defmic'
;;   - Use `defmacro'


;; 5.1 Define your own `mic' with `mic-defmic'
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   If you would like to add keywords, or to make some keywords more
;;   simple, you can define `filter' and apply it to `mic' (or `mic-core',
;;   and another `mic', any parent is allowed).


;; 5.1.1 What is a filter?
;; -----------------------

;;   The filter recieves one argument, `PLIST' (plist, property list), and
;;   returns `RETURNED-PLIST'.  It filters or transforms it into returned
;;   plist.  It is better to divide filters by every keyword, because of
;;   reusability.

;;   1. Each filter recieves 1 argument `PLIST', which is plist (property
;;      list).
;;   2. Each filter returns `RETURNED-PLIST', which is plist.
;;   3. `PLIST' is given by user or filter before.
;;   4. `PLIST' have feature name `:name' property.
;;   5. `RETURNED-PLIST' is passed to next filter or parent `mic' (`mic',
;;      `mic-core', or another).
;;   6. `RETURNED-PLIST' should have same value of `:name' property.
;;   7. The property only used by your filter should be removed in
;;      `RETURNED-PLIST'.

;;   Here is example:
;;   ,----
;;   | (defun my-filter-global-set-key-without-quote (plist)
;;   |   (let ((alist
;;   |          ;; Get value from your own keyword
;;   |          (plist-get plist :bind))
;;   |         sexps)
;;   |     (setq sexps
;;   |           ;; Transform each element
;;   |           (mapcar
;;   |            (lambda (arg)
;;   |              (let ((keys (car arg))
;;   |                    (command (cdr arg)))
;;   |                `(global-set-key (kbd ,keys) #',command)))
;;   |            alist))
;;   |     ;; Put sexps to `:eval' arguments
;;   |     (mic-plist-put-append plist :eval sexps)
;;   |     ;; Don't forget to delete your own keyword!
;;   |     ;; When forget it, parent recieves it and may cause unexpected result.
;;   |     (mic-plist-delete plist :bind)
;;   |     plist))
;;   |
;;   | ;; `defmic' defines new `mic' (see "Define mic with mic-defmic" section for more infomation)
;;   | (mic-defmic yourmic
;;   |   mic                                   ; Derived from `mic'
;;   |   :filters '(my-filter-global-set-key-without-quote))
;;   |
;;   | ;; Here is `yourmic' expression
;;   | (yourmic package-name
;;   |   ;; New keyword you added by `my-filter-global-set-key-without-quote'
;;   |   :bind
;;   |   (("M-a" . beginning-of-defun)
;;   |    ("M-e" . end-of-defun))
;;   |   ;; Of course keywords for `mic', which is original of `yourmic', is allowed.
;;   |   :hook ((after-init-hook . #'ignore)))
;;   |
;;   | ;; Then first `PLIST' is:
;;   | '( :name package-name
;;   |    :bind (("M-a" . beginning-of-defun)
;;   |           ("M-e" . end-of-defun))
;;   |    :hook ((after-init-hook . #'ignore)))
;;   |
;;   | ;; When you expand the sexp before, the filter you defined is called like:
;;   | (my-filter-global-set-key-without-quote
;;   |  '( :name package-name
;;   |     :bind (("M-a" . beginning-of-defun)
;;   |            ("M-e" . end-of-defun))
;;   |     :hook ((after-init-hook . #'ignore))))
;;   |
;;   | ;; It returns `RETURNED-PLIST':
;;   | '( :name package-name
;;   |    :hook ((after-init-hook function ignore))
;;   |    :eval
;;   |    ((global-set-key (kbd "M-a") #'beginning-of-defun)
;;   |     (global-set-key (kbd "M-e") #'end-of-defun)))
;;   |
;;   | ;; The `RETURNED-PLIST' is passed to a next filter if exists.
;;   | ;; You use only one filter in definition,
;;   | ;; so it is expanded to:
;;   | (mic package-name
;;   |   :hook ((after-init-hook . #'ignore))
;;   |   :eval
;;   |   ((global-set-key (kbd "M-a") #'beginning-of-defun)
;;   |    (global-set-key (kbd "M-e") #'end-of-defun)))
;;   `----


;; 5.1.2 Pre-defined filters
;; -------------------------

;;   Some pre-defined filter, unused by `mic' definition, are available in
;;   `mic-filter.el'.


;; * 5.1.2.1 Filters for package manager

;;   - `mic-filter-ell-get'
;;   - `mic-filter-straight'
;;   - `mic-filter-quelpa'
;;   For more infomation, see docstring of each filter.

;;   ,----
;;   | ;;;  el-get
;;   | (mic-defmic mic-with-el-get mic
;;   |   :filters '(mic-filter-el-get))
;;   |
;;   | (mic-with-el-get hydra
;;   |   :el-get ((hydra :repo "abo-abo/hydra" :fetcher github)))
;;   |
;;   | ;; Expanded to:
;;   | (mic hydra
;;   |   :eval-installation
;;   |   ((el-get-bundle hydra :repo "abo-abo/hydra" :fetcher github)))
;;   `----

;;   ,----
;;   | ;;;  quelpa
;;   | (mic-defmic mic-with-quelpa mic
;;   |   :filters '(mic-filter-quelpa))
;;   |
;;   | (mic-with-quelpa hydra
;;   |   :quelpa ((hydra :repo "abo-abo/hydra" :fetcher github)))
;;   |
;;   | ;; Expanded to:
;;   | (mic hydra
;;   |   :eval-installation
;;   |   ((quelpa
;;   |     '(hydra :repo "abo-abo/hydra" :fetcher github))))
;;   `----

;;   ,----
;;   | ;;;  straight
;;   | (mic-defmic mic-with-straight mic
;;   |   :filters '(mic-filter-straight))
;;   |
;;   | (mic-with-straight hydra
;;   |   :straight ((hydra :repo "abo-abo/hydra" :host github)))
;;   |
;;   | ;; Expanded to:
;;   | (mic hydra
;;   |   :eval-installation
;;   |   ((straight-use-package
;;   |     '(hydra :repo "abo-abo/hydra" :host github))))
;;   `----


;; * 5.1.2.2 Key definition

;;   - `mic-filter-define-key-general', `mic-filter-general-define-key'
;;   - `mic-filter-mykie'
;;   - `mic-filter-hydra'
;;   - `mic-filter-pretty-hydra', `mic-filter-pretty-hydra+'
;;   - `mic-filter-mode-hydra'

;;   Here is summaries and examples for these filters.  See a docstring and
;;   definition of each filter for more information.


;;   + 5.1.2.2.1 general.el

;;     [general.el] makes key definition more convenient.  There are some
;;     filters for integration with it:
;;     - `mic-filter-define-key-general'
;;     - `mic-filter-general-define-key'
;;     The both are expanded to `general-define-key' call.

;;     `mic-filter-define-key-general', which uses a `:define-key-general'
;;     keyword, is compatible with `:define-key' keyword.  In the other
;;     words, the syntax like `((keymap (key . function)...)...)' is
;;     allowed but `general-define-key' is used as backend.

;;     On the other hand, `mic-filter-general-define-key', which uses
;;     `:general-define-key' keyword, uses `general-define-key' syntax.  So
;;     you can use `:keymap' or `:prefix' keyword.  Each element of the
;;     value of `:general-define-key' is directly passed to
;;     `general-define-key'.

;;     ,----
;;     | (mic-defmic mic-with-define-key-general mic
;;     |   :filters
;;     |   '(mic-filter-define-key-general))
;;     |
;;     | (mic-with-define-key-general package-name
;;     |   :define-key-general
;;     |   ((keymap1
;;     |     ("C-d" . #'func1)
;;     |     ("C-q" . #'func2))
;;     |    (override
;;     |     ("C-a" . #'func3)
;;     |     ("C-e" . #'func4))))
;;     |
;;     | ;; Expanded to:
;;     | (mic package-name
;;     |   :eval
;;     |   ((general-define-key :keymaps 'keymap1
;;     |                        "C-d" (function func1)
;;     |                        "C-q" (function func2))
;;     |    (general-define-key :keymaps 'override
;;     |                        "C-a" (function func3)
;;     |                        "C-e" (function func4))))
;;     `----


;;     [general.el] <https://github.com/noctuid/general.el>


;;   + 5.1.2.2.2 Mykie.el

;;     [Mykie.el] is is multiplexer of key definition.  There is filter for
;;     mykie:
;;     - `mic-filter-mykie'

;;     `mic-filter-mykie', which uses a `:mykie' keyword, creates
;;     `mykie:define-key' sexp.  Each element of the value on `:mykie'
;;     keyword is a cons cell like `((keymap (key [:keyword function1]
;;     ...)...)...)'.  `car' of each element, which is keymap, and each
;;     element of `cdr' of each element of the value is passed to
;;     `mykie:define-key'.

;;     ,----

;;     `----

;;     ,----
;;     | (mic-defmic mic-with-filter-mykie mic
;;     |   :filters
;;     |   '(mic-filter-mykie))
;;     |
;;     | (mic-with-filter-mykie package-name
;;     |   :mykie
;;     |   ((global-map
;;     |     ("C-w" :default hydra-window-resizer/body :region kill-region))))
;;     |
;;     | ;; Expanded to:
;;     | (mic package-name
;;     |   :eval
;;     |   ((mykie:define-key global-map "C-w" :default hydra-window-resizer/body :region kill-region)))
;;     `----


;;     [Mykie.el] <https://github.com/yuutayamada/mykie-el>


;;   + 5.1.2.2.3 Hydra

;;     [Hydra] makes Emacs bindings stick around.  There is a filter for
;;     integration of Hydra:
;;     - `mic-filter-hydra'

;;     `mic-filter-hydra', which uses a `:hydra' keyword, creates
;;     `defhydra' sexp.  Each element of the value on the `:hydra' keyword
;;     is passed to `defhydra' directly.

;;     ,----
;;     | (mic-defmic mic-with-hydra mic
;;     |   :filters '(mic-filter-hydra))
;;     |
;;     | (mic-with-hydra package-name
;;     |   :hydra
;;     |   ;; Spacing induces good indent
;;     |   (( hydra-window-resizer ()
;;     |      ("p" shrink-window "shrink")
;;     |      ("n" enlarge-window "enlarge")
;;     |      ("f" enlarge-window-horizontally "enlarge-horizontally")
;;     |      ("b" shrink-window-horizontally "shrink-horizontally")
;;     |      ("<down>" shrink-window)
;;     |      ("<up>" enlarge-window)
;;     |      ("<right>" enlarge-window-horizontally)
;;     |      ("<left>" shrink-window-horizontally)
;;     |      ("q" nil "quit"))))
;;     |
;;     | ;; Expanded to:
;;     | (mic package-name
;;     |   :eval
;;     |   ((defhydra hydra-window-resizer nil
;;     |      ("p" shrink-window "shrink" :exit nil :cmd-name hydra-window-resizer/shrink-window :column nil)
;;     |      ("n" enlarge-window "enlarge")
;;     |      ("f" enlarge-window-horizontally "enlarge-horizontally")
;;     |      ("b" shrink-window-horizontally "shrink-horizontally")
;;     |      ("<down>" shrink-window)
;;     |      ("<up>" enlarge-window)
;;     |      ("<right>" enlarge-window-horizontally)
;;     |      ("<left>" shrink-window-horizontally)
;;     |      ("q" nil "quit"))))
;;     `----


;;     [Hydra] <https://github.com/abo-abo/hydra>


;;   + 5.1.2.2.4 pretty-hydra

;;     [Pretty Hydra] defines prettier hydra.  There is some filters for
;;     integration of it:
;;     - `mic-filter-pretty-hydra'
;;     - `mic-filter-pretty-hydra+'

;;     `mic-filter-pretty-hydra' uses `:pretty-hydra', whereas
;;     `mic-filter-pretty-hydra+' uses `:pretty-hydra+'.  Each element is
;;     passed to `pretty-hydra-define', which defines new hydra, or
;;     `pretty-hydra-define+', which appends to existing hydra if exist.
;;     The both have absolutely same syntax.  Each element is passed to each
;;     defining macros directly.

;;     ,----
;;     | (mic-defmic mic-with-pretty-hydra mic
;;     |   :filters '(mic-filter-pretty-hydra
;;     |              mic-filter-pretty-hydra+))
;;     |
;;     | ;;; `:pretty-hydra'
;;     | (mic-with-pretty-hydra package-name
;;     |   :pretty-hydra
;;     |   (( hydra-window-resizer ()
;;     |      ("Alphabet"
;;     |       (("p" shrink-window "shrink")
;;     |        ("n" enlarge-window "enlarge")
;;     |        ("f" enlarge-window-horizontally "enlarge-horizontally")
;;     |        ("b" shrink-window-horizontally "shrink-horizontally"))
;;     |       "Arrow"
;;     |       (("<down>" shrink-window)
;;     |        ("<up>" enlarge-window)
;;     |        ("<right>" enlarge-window-horizontally)
;;     |        ("<left>" shrink-window-horizontally))
;;     |       "Quit"
;;     |       ("q" nil "quit")))))
;;     |
;;     | ;; Expanded to:
;;     | (mic package-name
;;     |   :eval
;;     |   ((pretty-hydra-define hydra-window-resizer nil
;;     |      ("Alphabet"
;;     |       (("p" shrink-window "shrink")
;;     |        ("n" enlarge-window "enlarge")
;;     |        ("f" enlarge-window-horizontally "enlarge-horizontally")
;;     |        ("b" shrink-window-horizontally "shrink-horizontally"))
;;     |       "Arrow"
;;     |       (("<down>" shrink-window "shrink-window")
;;     |        ("<up>" enlarge-window "enlarge-window")
;;     |        ("<right>" enlarge-window-horizontally "enlarge-window-horizontally")
;;     |        ("<left>" shrink-window-horizontally "shrink-window-horizontally"))
;;     |       "Quit"
;;     |       ("q" nil "quit")))))
;;     |
;;     |
;;     | ;;; `:pretty-hydra+'
;;     | (mic-with-pretty-hydra package-name
;;     |   :pretty-hydra+
;;     |   (( hydra-window-resizer ()
;;     |      ("Vim-like"
;;     |       (("h" enlarge-window-horizontally "enlarge-horizontally")
;;     |        ("j" shrink-window "shrink")
;;     |        ("k" enlarge-window "enlarge")
;;     |        ("l" shrink-window-horizontally "shrink-horizontally"))))))
;;     |
;;     | ;; Expanded to:
;;     | (mic package-name
;;     |   :eval
;;     |   ((pretty-hydra-define+ hydra-window-resizer nil
;;     |      ("Vim-like"
;;     |       (("h" enlarge-window-horizontally "enlarge-horizontally")
;;     |        ("j" shrink-window "shrink")
;;     |        ("k" enlarge-window "enlarge")
;;     |        ("l" shrink-window-horizontally "shrink-horizontally"))))))
;;     `----


;;     [Pretty Hydra]
;;     <https://github.com/jerrypnz/major-mode-hydra.el#pretty-hydra>


;;   + 5.1.2.2.5 major-mode-hydra

;;     [Major Mode Hydra] defines major-mode specific hydra function,
;;     `major-mode-hydra'.  There is a filter for integration of it:
;;     - `mic-filter-mode-hydra'

;;     `mic-filter-mode-hydra' uses a `:mode-hydra' keyword.  Each element
;;     of the value of the keyword is passed to `major-mode-hydra-define'
;;     directly.

;;     ,----
;;     | (mic-defmic mic-with-mode-hydra mic
;;     |   :filters '(mic-filter-mode-hydra))
;;     |
;;     | (mic-with-mode-hydra package-name
;;     |   :mode-hydra
;;     |   (( c-mode (:title "C Mode" :quit-key "q")
;;     |      ("Alphabet"
;;     |       (("p" shrink-window "shrink")
;;     |        ("n" enlarge-window "enlarge")
;;     |        ("f" enlarge-window-horizontally "enlarge-horizontally")
;;     |        ("b" shrink-window-horizontally "shrink-horizontally"))
;;     |       "Arrow"
;;     |       (("<down>" shrink-window)
;;     |        ("<up>" enlarge-window)
;;     |        ("<right>" enlarge-window-horizontally)
;;     |        ("<left>" shrink-window-horizontally))))))
;;     |
;;     | ;; Expanded to:
;;     | (mic package-name
;;     |   :eval
;;     |   ((major-mode-hydra-define c-mode
;;     |      (:title "C Mode" :quit-key "q")
;;     |      ("Alphabet"
;;     |       (("p" shrink-window "shrink")
;;     |        ("n" enlarge-window "enlarge")
;;     |        ("f" enlarge-window-horizontally "enlarge-horizontally")
;;     |        ("b" shrink-window-horizontally "shrink-horizontally"))
;;     |       "Arrow"
;;     |       (("<down>" shrink-window "shrink-window")
;;     |        ("<up>" enlarge-window "enlarge-window")
;;     |        ("<right>" enlarge-window-horizontally "enlarge-window-horizontally")
;;     |        ("<left>" shrink-window-horizontally "shrink-window-horizontally"))))))
;;     `----


;;     [Major Mode Hydra]
;;     <https://github.com/jerrypnz/major-mode-hydra.el#major-mode-hydra>


;; * 5.1.2.3 Alternative of filters

;;   + 5.1.2.3.1 Hook

;;     - `mic-filter-hook-quote'
;;     This is almost same as `mic-filter-hook', but `cdr' of each element
;;     of the value should not be quoted.  `:hook-quote' is used as
;;     keyword.


;; 5.1.3 Helper for defining a filter
;; ----------------------------------

;;   There are some helpers for defining a filter.


;; * 5.1.3.1 Utilities

;;   Usually, a filter proceeds filtering by 4 steps:
;;   1. Get data on a specific keyword in `PLIST'
;;   2. Convert data to sexp
;;   3. Append the sexp to value on `:eval' in `PLIST'
;;   4. Delete the specific keyword from `PLIST'

;;   There are some macros to help step 3. and 4. in `mic-utils.el'.
;;   - `mic-plist-put-append', which helps step 3., takes three arguments,
;;     `PLIST', `PROP', which means keyword, and `VAL'.  It get a value on
;;     `PROP' in `PLIST', and appends `VAL' to the value.
;;   - `mic-plist-delete', which helps step 4., takes one obligatory
;;     argument `PLIST', and extra arguments `PROPS'.  It removes `PROPS'
;;     keywords from `PLIST' and return it.


;; * 5.1.3.2 `deffilter'

;;   To define a simple filter or to modify an existing filter, you can use
;;   `mic-deffilter-*' macros in `mic-deffilter.el'.  See each macro
;;   definition and docstring for more information.

;;   `mic-deffilter-alias'
;;         Induce alias keyword.
;;         ,----
;;         | (mic-deffilter-alias example-filter-alias :alias :origin)
;;         |
;;         | (example-filter-alias '(:alias "Hello"))
;;         | ;; =>
;;         | (:origin "Hello")
;;         `----
;;   `mic-deffilter-const'
;;         Put constant value on keyword.
;;         ,----
;;         | (mic-deffilter-const example-filter-const
;;         |   "Optional docstring."
;;         |   :eval '((message "Hello")))
;;         |
;;         | ;; Add a :eval keyword when it does not exist.
;;         | (example-filter-const '(:other-keyword "Hi"))
;;         | ;; =>
;;         | (:other-keyword "Hi" :eval ((message "Hello")))
;;         |
;;         | ;; Overwrite when a :eval keyword exists.
;;         | (example-filter-const '(:eval ((message "Good bye")) :other-keyword "Hi"))
;;         | ;; =>
;;         | (:eval ((message "Hello")) :other-keyword "Hi")
;;         `----
;;   `mic-deffilter-const-append'
;;         Append constant value on keyword.
;;         ,----
;;         | (mic-deffilter-const-append example-filter-const-append
;;         |   :eval '((message "Hello")))
;;         |
;;         | ;; Same as `mic-deffilter-const' when any :eval keyword does not exist.
;;         | (example-filter-const-append '(:other-keyword "Hi"))
;;         | ;; =>
;;         | (:other-keyword "Hi" :eval ((message "Hello")))
;;         |
;;         | ;; Append the value when the a :eval keyword exists.
;;         | (example-filter-const-append '(:eval ((message "Good bye")) :other-keyword "Hi"))
;;         | ;; =>
;;         | (:eval ((message "Good bye") (message "Hello")) :other-keyword "Hi")
;;         `----
;;   `mic-deffilter-ignore'
;;         Just remove value on keyword.
;;         ,----
;;         | (mic-deffilter-ignore example-filter-ignore
;;         |   :ignore-me)
;;         |
;;         | (example-filter-ignore '(:ignore-me "Ignored" :remain-me "Remained"))
;;         | ;; =>
;;         | (:remain-me "Remained")
;;         `----
;;   `mic-deffilter-nonlist-to-list'
;;         If value is not list, wrap it into list.
;;         ,----
;;         | (mic-deffilter-nonlist-to-list example-filter-nonlist-to-list
;;         |   :package)
;;         |
;;         | (example-filter-nonlist-to-list '(:package t))
;;         | ;; =>
;;         | (:package (t))
;;         `----
;;   `mic-deffilter-replace-keyword-append'
;;         From an existing filter, define a new filter which uses another
;;         keywords as input and output.  Value is appended to the keyword
;;         for output.
;;         ,----
;;         | ;; Original filter: `mic-filter-mykie'
;;         | (mic-filter-mykie '(:mykie ((global-map ("C-a" :default beginning-of-line)))))
;;         | ;; =>
;;         | (:eval ((mykie:define-key global-map "C-a" :default beginning-of-line)))
;;         |
;;         |
;;         | (mic-deffilter-replace-keyword-append example-filter-replace-keyword-append
;;         |   mic-filter-mykie
;;         |   :mykie-after-load :mykie
;;         |   '((:eval . :eval-after-load)))
;;         |
;;         | ;; An input keyword and an output keyword is replaced
;;         | (example-filter-replace-keyword-append '(:mykie-after-load ((global-map ("C-a" :default beginning-of-line)))))
;;         | ;; =>
;;         | (:eval-after-load ((mykie:define-key global-map "C-a" :default beginning-of-line)))
;;         `----
;;   `mic-deffilter-convert-after-load'
;;         From an existing filter, define a new filter which outputs an
;;         `:eval-after-load' keyword instead of `:eval'.  It is same as
;;         `(mic-deffilter-replace-keyword-append name filter old-keyword
;;         new-keyword '((:eval . :eval-after-load)))'.
;;         ,----
;;         | ;; Original filter: `mic-filter-mykie'
;;         | (mic-filter-mykie '(:mykie ((global-map ("C-a" :default beginning-of-line)))))
;;         | ;; =>
;;         | (:eval ((mykie:define-key global-map "C-a" :default beginning-of-line)))
;;         |
;;         |
;;         | (mic-deffilter-convert-after-load example-filter-convert-after-load
;;         |   mic-filter-mykie
;;         |   :mykie-after-load :mykie)
;;         |
;;         | ;; An input keyword and an output keyword is replaced
;;         | (example-filter-convert-after-load '(:mykie-after-load ((global-map ("C-a" :default beginning-of-line)))))
;;         | ;; =>
;;         | (:eval-after-load ((mykie:define-key global-map "C-a" :default beginning-of-line)))
;;         `----
;;   `mic-deffilter-t-to-name'
;;         Replace `t' with feature name in a list keyword.
;;         ,----
;;         | (mic-deffilter-t-to-name example-filter-t-to-name
;;         |   :replace)
;;         |
;;         |  ;; :name keyword is needed in addition to :replace keyword
;;         | (example-filter-t-to-name '(:name feature-name :replace (1 2 3 t 5 6 t)))
;;         | ;; =>
;;         | (:name feature-name :replace (1 2 3 feature-name 5 6 feature-name))
;;         `----
;;   `mic-deffilter-validate'
;;         Return a recieved plist except that it validates and sieves
;;         keyword in the plist to confirm the returned plist has no
;;         invalid keywords.
;;         ,----
;;         | (mic-deffilter-validate example-filter-validate
;;         |   :name :key1 :key2)
;;         |
;;         | (example-filter-validate '(:name feature-name :key1 "Hello" :key2 "Hi" :key3 "Bad" :key4 "Sad"))
;;         | ;; =>
;;         | (:name feature-name :key1 "Hello" :key2 "Hi")
;;         | ;; In addition, warnings are displayed like:
;;         | ;; Warning (Emacs): 'mic' feature-name: The keyword :key3 is not allowed by filter 'example-filter-validate'
;;         | ;; Warning (Emacs): 'mic' feature-name: The keyword :key4 is not allowed by filter 'example-filter-validate'
;;         `----


;; 5.1.4 Define `mic' with `mic-defmic'
;; ------------------------------------

;;   `mic-defmic' recieves arguments: `NAME', `PANRENT', optional
;;   `DOCSTRING', keyword argument `FILTERS'.  `NAME' is your new `mic'
;;   macro name.  `PARENT' is parent `mic', which recieves `RETURNED-PLIST'
;;   at last.  `FILTERS' is list of your filters.  When your `mic' recieves
;;   plist, the plist is filtered by all of your `FILTERS' in order, then
;;   the plist is passed to `PARENT'.

;;   Here is example:
;;   ,----
;;   | ;; Define `mymic'
;;   | (mic-defmic mymic
;;   |   ;; Parent is here.  You can also use `mic-core'.
;;   |   mic
;;   |   :filters
;;   |   '(my-filter-global-set-key-without-quote
;;   |     ;; You can add other filters below
;;   |     )
;;   |   ;; You can comment out the line below to catch, warn and ignore errors.
;;   |   ;; :error-protection? t
;;   |   )
;;   |
;;   | ;; Then you can use `mymic' like:
;;   | (mymic simple
;;   |   :bind
;;   |   (("C-d" . delete-forward-char)
;;   |    ("C-x l" . toggle-truncate-lines))
;;   |   ;; Of course parent keywords are accepted.
;;   |   :custom
;;   |   ((kill-whole-line . t)
;;   |    (set-mark-command-repeat-pop . t)
;;   |    (mark-ring-max . 50)))
;;   |
;;   | ;; Expanded to:
;;   | (mic simple
;;   |   :custom
;;   |   ((kill-whole-line . t)
;;   |    (set-mark-command-repeat-pop . t)
;;   |    (mark-ring-max . 50))
;;   |   :eval
;;   |   ((global-set-key (kbd "C-d") #'delete-forward-char)
;;   |    (global-set-key (kbd "C-x l") #'toggle-truncate-lines)))
;;   `----

;;   When you would like to use `mic-core' as `PARENT',
;;   `mic-filter-core-validate' is useful to validate plist.  *Please put
;;   it tail of `FILTERS' if you use it.*


;; * 5.1.4.1 Error protection

;;   If you want your `mic' to catch, warn and dismiss errors and to
;;   continue evaluation, set `:error-protection?' `t'.
;;   ,----
;;   | (mic-defmic mymic-with-error-protection
;;   |   ;; Parent is here.  You can also use `mic-core'.
;;   |   mic
;;   |   :filters
;;   |   '(my-filter-global-set-key-without-quote)
;;   |   :error-protection? t)
;;   |
;;   | (mymic-with-error-protection simple
;;   |   :bind
;;   |   (("C-d" . delete-forward-char)
;;   |    ("C-x l" . toggle-truncate-lines))
;;   |   ;; Of course parent keywords are accepted.
;;   |   :custom
;;   |   ((kill-whole-line . t)
;;   |    (set-mark-command-repeat-pop . t)
;;   |    (mark-ring-max . 50)))
;;   |
;;   | ;; Expanded to:
;;   | (condition-case-unless-debug error      ; Catch error
;;   |     (mic simple
;;   |       :custom
;;   |       ((kill-whole-line . t)
;;   |        (set-mark-command-repeat-pop . t)
;;   |        (mark-ring-max . 50))
;;   |       :eval
;;   |       ((global-set-key (kbd "C-d") (function delete-forward-char))
;;   |        (global-set-key (kbd "C-x l") (function toggle-truncate-lines))))
;;   |   ;; Warn caught error but continue evaluation
;;   |   (error
;;   |    (warn "`%s' %s: evaluation error: %s" 'mymic-with-error-protection 'simple
;;   |          (error-message-string error))))
;;   `----


;; * 5.1.4.2 Adopt a parent other than `mic', `mic-core' and its derivation

;;   You can use other configuration managers, such as [use-package] and
;;   [leaf.el].  However, filters defined by `mic' output keyword for `mic'
;;   family, such as `:eval', `:eval-after-load'.  So you should tell
;;   `mic-defmic' how to adapt outputs to its parent by `:adapter' option.
;;   The adapter takes one argument `PLIST', and returns a list to pass to
;;   the parent.

;;   Two adapter are pre-defined:
;;   `mic-adapter-use-package'
;;         Adapter for `use-package'.
;;   `mic-adapter-leaf'
;;         Adapter for `leaf'.

;;   ,----
;;   | (mic-defmic mic-with-use-package use-package
;;   |   :filters '(mic-filter-define-key-with-feature)
;;   |   :adapter #'mic-adapter-use-package)
;;   |
;;   | (mic-with-use-package feature-name
;;   |   :define-key-with-feature
;;   |   ((org
;;   |     (org-mode-map
;;   |      ("M-a" . #'feature-name-command))))
;;   |   ;; You can use `use-package' feature
;;   |   :bind
;;   |   (("M-a" . beginning-of-defun)
;;   |    ("M-e" . end-of-defun)))
;;   |
;;   | ;; Expanded to:
;;   | (use-package feature-name
;;   |   :bind
;;   |   (("M-a" . beginning-of-defun)
;;   |    ("M-e" . end-of-defun))
;;   |   ;; :defer is needed to wrap :config section around `eval-after-load'
;;   |   :defer t
;;   |   :init
;;   |   (with-eval-after-load 'org
;;   |     (define-key org-mode-map (kbd "M-a") (function feature-name-command))))
;;   `----


;;   [use-package] <https://github.com/jwiegley/use-package>

;;   [leaf.el] <https://github.com/conao3/leaf.el>


;; 5.2 Define your own `mic' with `defmacro'
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   When you read here, you should know `defmacro'.  You can do anything
;;   with `defmacro'.  `mic-defmic' is easy way to define your `mic', but
;;   may be not enough for you, because of restriction.  Then *I RECOMMEND
;;   to use `defmacro'*.  I am looking forward to seeing your `mic' defined
;;   by `defmacro'!


;; 6 Alternative
;; =============

;;   There are some alternatives:
;;   - [`use-package']
;;   - [`leaf']

;;   They are more easy to use, but sometimes have less expressive ability.
;;   `mic' is more simple and has more expressive ability, but sometimes
;;   more redundant.  It is just your preference.

;;   In addition, they are customizable, while `mic' is not customizable,
;;   but re-definable.  You can define your own `mic' according to your
;;   preference, with `mic' help.  Of course you can define your own `mic'
;;   with `use-package' or `leaf' as backend.


;; [`use-package'] <https://github.com/jwiegley/use-package>

;; [`leaf'] <https://github.com/conao3/leaf.el>


;; 7 Contribute
;; ============

;;   When you think you would like to share your filter or your own `mic',
;;   use GitHub Discussion.  Of course your `mic' defined by
;;   `defmacro'.  Any issue is welcome.


;; 8 License
;; =========

;;   This package is licensed by GPLv3. See [LICENSE].


;; [LICENSE] <file:LICENSE>

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'mic-deffilter)
(require 'mic-utils)

(defgroup mic ()
  "Minimal and combinable configuration manager."
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
(defmacro mic-defmic (name parent docstring &rest definition-plist)
  "Define new `mic' named NAME derived from PARENT.
DOCSTRING is docuent of it, and DEFINITION-PLIST is main definition.

FILTERS is list of filter, which recieve plist and return plist.
The recieved plist has property `:name', which is package name.
It also has other properties from other filter before.

If ERROR-PROTECTION? is non-nil, any errors are catched, warned and ignored.
Otherwise, when errors occur, loading your init.el stops.

The defined macro recieves two arguments, NAME and PLIST.
PLIST is filtered by each FILTERS in order and passed to PARENT.

\(fn NAME PARENT [DOCSTRING] &key FILTERS ERROR-PROTECTION? ADAPTER INPUTTER)"
  (declare (indent defun)
           (doc-string 2))
  (unless (stringp docstring)
    (push docstring definition-plist)
    (setq docstring nil))
  (let ((allowed-keywords '(:filters :error-protection? :adapter :inputter))
        (temp-plist definition-plist)
        key)
    (while (setq key (pop temp-plist))
      (pop temp-plist)
      (unless (memq key allowed-keywords)
        (error "Keyword %s is not allowed in `mic-defmic'" key))))

  (let* ((filters (eval (plist-get definition-plist :filters)))
         (error-protection? (eval (plist-get definition-plist :error-protection?)))
         (adapter (or (eval (plist-get definition-plist :adapter))
                      #'identity))
         (inputter (or (eval (plist-get definition-plist :inputter))
                       #'identity)))
    `(defmacro ,name (feature-name &rest input)
       ,(or docstring
            (format "`mic' alternative defined by `mic-defmic'.
Configure about FEATURE-NAME by INPUT.

Information:
- Filters:
%s
- Parent: `%s'
- Error protection: %s
- Adapter: `%s'
- Inputter: `%s'

For more information, see `mic-defmic'."
                    (mapconcat
                     (lambda (arg)
                       (concat "  - `" (pp-to-string arg) "'"))
                     filters
                     "\n")
                    parent
                    error-protection?
                    adapter
                    inputter))
       (declare (indent defun))
       ,(if error-protection?
            `(let ((plist
                    (condition-case-unless-debug error
                        (,inputter input)
                      (error
                       (warn "`%s' %s: inputter %s error: %s"
                             ',name feature-name ',inputter (error-message-string error))))))
               (condition-case-unless-debug error
                   (mic-apply-filter plist feature-name
                     ,@filters)
                 (error
                  (warn "`%s' %s: macro expansion error: %s"
                        ',name feature-name (error-message-string error))))
               ,(let ((error (make-symbol "error")))
                  `(backquote
                    ,(list
                      'condition-case-unless-debug error
                      (list
                       parent
                       ',name
                       (list
                        '\,@
                        `(,adapter plist)))
                      `(error
                        ,(list
                          'warn "`%s' %s: evaluation error: %s"
                          `',name '',feature-name
                          `(error-message-string ,error)))))))
          `(let ((plist (,inputter input)))
             (mic-apply-filter plist feature-name
               ,@filters)
             (backquote
              ,(list
                parent
                ',feature-name
                (list
                 '\,@
                 `(,adapter plist)))))))))



(defsubst mic-make-sexp-autoload-interactive (name list)
  "Create `autoload' sexp from LIST and NAME.
Each element of LIST is interactive function which should be autoloaded,
and NAME is feature."
  (mapcar
   (lambda (arg)
     `(autoload ',arg ,(symbol-name name) nil t))
   list))

(defun mic-filter-autoload-interactive (plist)
  "Append sexp from value :autoload-interactive to value of :eval on PLIST.
The sexp is generated by `mic-make-sexp-autoload-interactive'."
  (mic-plist-put-append plist :eval-installation
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
     `(autoload ',arg ,(symbol-name name)))
   list))

(defun mic-filter-autoload-noninteractive (plist)
  "Append sexp from value of :autoload-noninteractive to value of :eval on PLIST.
The sexp is generated by `mic-make-sexp-autoload-noninteractive'."
  (mic-plist-put-append plist :eval-installation
                        (mic-make-sexp-autoload-noninteractive
                         (plist-get plist :name)
                         (plist-get plist :autoload-noninteractive)))
  (mic-plist-delete plist :autoload-noninteractive)
  plist)

(defsubst mic-make-sexp-auto-mode (alist)
  "Create sexp adding to `auto-mode-alist' from ALIST and NAME.
Evaluating the sexp make ALIST merged to `auto-mode-alist'."
  (mapcar
   (lambda (arg)
     `(add-to-list 'auto-mode-alist ',arg))
   alist))

(defun mic-filter-auto-mode (plist)
  "Append sexp from value of :auto-mode to value of :eval on PLIST.
The sexp is generated by `mic-make-sexp-auto-mode'."
  (mic-plist-put-append plist :eval-installation
                        (mic-make-sexp-auto-mode
                         (plist-get plist :auto-mode)))
  (mic-plist-delete plist :auto-mode)
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
`car' of each element is HOOK, and `cdr' is FUNCTION.
FUNCTION should be quoted."
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

(defsubst mic-make-sexp-require (list)
  "Create `require' sexp from LIST.
Each element is feature symbol."
  (mapcar
   (lambda (arg)
     `(require ',arg))
   list))

(defun mic-filter-require (plist)
  "Append sexp from value of :require to value of :eval on PLIST.
Sexp is generated by `mic-make-sexp-require'."
  (mic-plist-put-append plist :eval
                        (mic-make-sexp-require
                         (plist-get plist :require)))
  (mic-plist-delete plist :require)
  plist)

(defsubst mic-make-sexp-require-after (alist)
  "Create `require' sexp with `with-eval-after-load' from ALIST.
`car' of each element is feature symbol needed to load,
and `cdr' of each element is list of feature symbols `require'd after the `car'."
  (mapcar
   (lambda (arg)
     (append (list 'with-eval-after-load `',(car arg))
             (mic-make-sexp-require (cdr arg))))
   alist))

(defun mic-filter-require-after (plist)
  "Append sexp from value of :require-after to value of :eval on PLIST.
Sexp is generated by `mic-make-sexp-require-after'."
  (mic-plist-put-append plist :eval
                        (mic-make-sexp-require-after
                         (plist-get plist :require-after)))
  (mic-plist-delete plist :require-after)
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
    mic-filter-auto-mode
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
    mic-filter-require
    mic-filter-require-after
    mic-filter-core-validate))

(provide 'mic)
;;; mic.el ends here
