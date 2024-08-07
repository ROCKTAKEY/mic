;;; mic-test.el --- Test for mic

;; Copyright (C) 2022  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>

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

;; Test for mic

;;; Code:

(require 'ert)

(require 'undercover)
(undercover "*.el"
            (:report-format 'codecov)
            (:report-file "coverage-final.json")
            (:send-report nil))

(require 'mic)
(require 'mic-utils)

(defmacro mic-ert-macroexpand-1 (name &rest args)
  "Define test named NAME.
The test compare macro expandation of `car' of each element of ARGS with `cdr' of it.
The test defined by this expands macro once."
  (declare (indent defun))
  `(ert-deftest ,name ()
     ,@(mapcar
        (lambda (arg)
          `(should (equal (macroexpand-1 ',(car arg))
                          ',(cdr arg))))
        args)))

(defmacro mic-ert-macroexpand-2 (name &rest args)
  "Define test named NAME.
The test compare macro expandation of `car' of each element of ARGS with `cdr' of it.
The test defined by this expands macro twice."
  (declare (indent defun))
  `(ert-deftest ,name ()
     ,@(mapcar
        (lambda (arg)
          `(should (equal (macroexpand-1 (macroexpand-1 ',(car arg)))
                          ',(cdr arg))))
        args)))



(mic-ert-macroexpand-2 mic-autoload-interactive
  ((mic feature-name
     :autoload-interactive
     (find-file
      write-file))
   . (prog1 'feature-name
       (autoload 'find-file "feature-name" nil t)
       (autoload 'write-file "feature-name" nil t))))

(mic-ert-macroexpand-2 mic-autoload-noninteractive
  ((mic feature-name
     :autoload-noninteractive
     (cl-map
      cl-mapcar))
   . (prog1 'feature-name
       (autoload 'cl-map "feature-name")
       (autoload 'cl-mapcar "feature-name"))))

(mic-ert-macroexpand-2 mic-auto-mode
  ((mic feature-name
     :auto-mode
     (("\\.html?\\'" . web-mode)
      ("\\.css\\'" . web-mode)))
   . (prog1 'feature-name
       (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
       (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode)))))

(mic-ert-macroexpand-2 mic-custom
  ((mic feature-name
     :custom
     ((a . 1)
      (b . (+ 1 2))))
   . (prog1 'feature-name
       (customize-set-variable 'a 1)
       (customize-set-variable 'b
                               (+ 1 2)))))

(mic-ert-macroexpand-2 mic-custom-after-load
  ((mic feature-name
     :custom-after-load
     ((a . 1)
      (b . (+ 1 2))))
   . (prog1 'feature-name
       (with-eval-after-load 'feature-name
         (customize-set-variable 'a 1)
         (customize-set-variable 'b
                                 (+ 1 2))))))

(mic-ert-macroexpand-2 mic-declare-function
  ((mic feature-name
     :declare-function
     (find-file
      write-file))
   . (prog1 'feature-name
       (declare-function find-file "ext:feature-name")
       (declare-function write-file "ext:feature-name"))))

(mic-ert-macroexpand-2 mic-define-key
  ((mic feature-name
     :define-key
     ((global-map
       ("C-t" . #'other-window)
       ("C-n" . #'next-window))
      (prog-mode-map
       ("M-a" . #'beginning-of-buffer)
       ("M-e" . #'end-of-buffer))))
   . (prog1 'feature-name
       (define-key global-map (kbd "C-t") #'other-window)
       (define-key global-map (kbd "C-n") #'next-window)
       (define-key prog-mode-map (kbd "M-a") #'beginning-of-buffer)
       (define-key prog-mode-map (kbd "M-e") #'end-of-buffer))))

(mic-ert-macroexpand-2 mic-define-key-after-load
  ((mic feature-name
     :define-key-after-load
     ((c-mode-map
       ("C-t" . #'other-window)
       ("C-n" . #'next-window))
      (c++-mode-map
       ("M-a" . #'beginning-of-buffer)
       ("M-e" . #'end-of-buffer))))
   . (prog1 'feature-name
       (with-eval-after-load 'feature-name
         (define-key c-mode-map (kbd "C-t") #'other-window)
         (define-key c-mode-map (kbd "C-n") #'next-window)
         (define-key c++-mode-map (kbd "M-a") #'beginning-of-buffer)
         (define-key c++-mode-map (kbd "M-e") #'end-of-buffer)))))

(mic-ert-macroexpand-2 mic-define-key-with-feature
  ((mic feature-name
     :define-key-with-feature
     ((cc-mode
       (c-mode-map
        ("C-t" . #'other-window)
        ("C-n" . #'next-window))
       (c++-mode-map
        ("M-a" . #'beginning-of-buffer)
        ("M-e" . #'end-of-buffer)))
      (python
       (python-mode-map
        ("C-t" . #'python-check)))))
   . (prog1 'feature-name
       (with-eval-after-load 'cc-mode
         (define-key c-mode-map (kbd "C-t") #'other-window)
         (define-key c-mode-map (kbd "C-n") #'next-window)
         (define-key c++-mode-map (kbd "M-a") #'beginning-of-buffer)
         (define-key c++-mode-map (kbd "M-e") #'end-of-buffer))
       (with-eval-after-load 'python
         (define-key python-mode-map (kbd "C-t") #'python-check)))))

(mic-ert-macroexpand-2 mic-defvar-noninitial
  ((mic feature-name
     :defvar-noninitial
     (skk-jisyo
      skk-use-azik))
   . (prog1 'feature-name
       (defvar skk-jisyo)
       (defvar skk-use-azik))))

(mic-ert-macroexpand-2 mic-eval
  ((mic feature-name
     :eval
     ((message "Hello")
      (message "World")))
   . (prog1 'feature-name
       (message "Hello")
       (message "World"))))

(mic-ert-macroexpand-2 mic-eval-after-load
  ((mic feature-name
     :eval-after-load
     ((message "Hello")
      (message "World")))
   . (prog1 'feature-name
       (with-eval-after-load 'feature-name
         (message "Hello")
         (message "World")))))

(mic-ert-macroexpand-2 mic-eval-after-others
  ((mic feature-name
     :custom
     ((skk-jisyo . "~/skk-jisyo"))
     :eval
     ((message "before")
      (message "custom"))
     :eval-after-others
     ((message "after")
      (message "custom")))
   . (prog1 'feature-name
       (message "before")
       (message "custom")
       (customize-set-variable
        'skk-jisyo
        "~/skk-jisyo")
       (message "after")
       (message "custom"))))

(mic-ert-macroexpand-2 mic-eval-after-others-after-load
  ((mic feature-name
     :custom-after-load
     ((skk-jisyo . "~/skk-jisyo"))
     :eval-after-load
     ((message "before")
      (message "custom"))
     :eval-after-others-after-load
     ((message "after")
      (message "custom")))
   . (prog1 'feature-name
       (with-eval-after-load 'feature-name
         (message "before")
         (message "custom")
         (customize-set-variable
          'skk-jisyo
          "~/skk-jisyo")
         (message "after")
         (message "custom")))))

(mic-ert-macroexpand-2 mic-eval-before-all
  ((mic feature-name
     :custom
     ((skk-jisyo . "~/skk-jisyo"))
     :eval
     ((message "before")
      (message "custom"))
     :eval-before-all
     ((message "before")
      (message "all")))
   . (prog1 'feature-name
       (message "before")
       (message "all")
       (message "before")
       (message "custom")
       (customize-set-variable
        'skk-jisyo
        "~/skk-jisyo"))))

(mic-ert-macroexpand-2 mic-face
  ((mic feature-name
     :face
     ((aw-leading-char-face
       . ((t (:foreground "red" :height 10.0))))
      (aw-mode-line-face
       . ((t (:background "#006000" :foreground "white" :bold t))))))
   . (prog1 'feature-name
       (custom-set-faces
        '(aw-leading-char-face
          ((t (:foreground "red" :height 10.0))))
        '(aw-mode-line-face
          ((t (:background "#006000" :foreground "white" :bold t))))))))

(mic-ert-macroexpand-2 mic-hook
  ((mic feature-name
     :hook
     ((after-init-hook . #'ignore)
      (prog-mode-hook . (lambda ()))))
   . (prog1 'feature-name
       (add-hook 'after-init-hook #'ignore)
       (add-hook 'prog-mode-hook (lambda ())))))

(mic-ert-macroexpand-2 mic-package
  ((mic feature-name
     :package
     (package-1
      package-2))
   . (prog1 'feature-name
       (unless (package-installed-p 'package-1)
         (when (assq 'package-1 package-archive-contents)
           (ignore-errors
             (package-install 'package-1)))
         (unless (package-installed-p 'package-1)
           (package-refresh-contents)
           (condition-case _
               (package-install 'package-1)
             (error
              (warn "Package %s is not found" 'package-1)))))
       (unless (package-installed-p 'package-2)
         (when (assq 'package-2 package-archive-contents)
           (ignore-errors
             (package-install 'package-2)))
         (unless (package-installed-p 'package-2)
           (package-refresh-contents)
           (condition-case _
               (package-install 'package-2)
             (error
              (warn "Package %s is not found" 'package-2))))))))

(mic-ert-macroexpand-2 mic-require
  ((mic feature-name
     :require
     (feat1
      feat2))
   . (prog1 'feature-name
       (require 'feat1)
       (require 'feat2))))

(mic-ert-macroexpand-2 mic-require-after
  ((mic feature-name
     :require-after
     ((feat-after1
       . (feat1  feat2))
      (feat-after2
       feat3
       feat4)))
   . (prog1 'feature-name
       (with-eval-after-load 'feat-after1
         (require 'feat1)
         (require 'feat2))
       (with-eval-after-load 'feat-after2
         (require 'feat3)
         (require 'feat4)))))



(mic-ert-macroexpand-1 mic-apply-filter
  ((mic-apply-filter plist name-var
     filter1
     filter2
     filter3)
   . (progn
       (mic-plist-put plist :name name-var)
       (setq plist
             (thread-last plist filter1 filter2 filter3))
       (mic-plist-delete plist :name))))

(mic-ert-macroexpand-1 mic-defmic-macroexpand-1
  ((mic-defmic macro-name parent-name
     "docstring"
     :filters '(filter1 filter2))
   . (defmacro macro-name (feature-name &rest input)
       "docstring"
       (declare (indent defun))
       (let ((plist (identity input)))
         (mic-apply-filter plist feature-name
           filter1 filter2)
         (backquote
          (parent-name ,feature-name ,@(identity plist))))))
  ((mic-defmic macro-name parent-name
     :filters '(filter1 filter2))
   . (defmacro macro-name (feature-name &rest input)
       "`mic' alternative defined by `mic-defmic'.
Configure about FEATURE-NAME by INPUT.

Information:
- Filters:
  - `filter1'
  - `filter2'
- Parent: `parent-name'
- Error protection: nil
- Adapter: `identity'
- Inputter: `identity'

For more information, see `mic-defmic'."
       (declare (indent defun))
       (let ((plist (identity input)))
         (mic-apply-filter plist feature-name filter1 filter2)
         (backquote
          (parent-name ,feature-name ,@(identity plist)))))))



(mic-deffilter-const-append mic-test-filter-const-1
  :foo '(1)
  :bar '(2 3))

(mic-deffilter-const-append mic-test-filter-const-2
  :foo '(4 5)
  :baz '(6 7))

(mic-defmic mic-test-mic-defmic-filters parent-name
  :filters '(mic-test-filter-const-1 mic-test-filter-const-2))

(mic-ert-macroexpand-1 mic-defmic-filters
  ((mic-test-mic-defmic-filters feature-name
     :foo (123)
     :bar (456))
   . (parent-name feature-name
                  :foo
                  (123 1 4 5)
                  :bar
                  (456 2 3)
                  :baz
                  (6 7))))

(mic-defmic mic-test-mic-defmic-error-protection parent-name
  :filters '(mic-test-filter-const-1 mic-test-filter-const-2)
  :error-protection? t)

(ert-deftest mic-defmic-error-protection ()
  (should
   (pcase (macroexpand-1
           '(mic-test-mic-defmic-error-protection feature-name
              :foo (123)
              :bar (456)))
     (`(condition-case-unless-debug ,error
           (parent-name feature-name :foo
                        (123 1 4 5)
                        :bar
                        (456 2 3)
                        :baz
                        (6 7))
         (error
          (warn "`%s' %s: evaluation error: %s" 'mic-test-mic-defmic-error-protection 'feature-name
                (error-message-string ,error))))
      t)
     (_ nil))))

(mic-defmic mic-test-mic-defmic-adapter parent-name
  :filters '(mic-test-filter-const-1 mic-test-filter-const-2)
  :adapter
  (lambda (plist)
    "Replace :foo with :hoge in PLIST.
Then, duplicate value on :bar."
    (let (result)
      (while plist
        (let ((key (pop plist))
              (value (pop plist)))
          (when (eq key :foo)
            (setq key :hoge))
          (push key result)
          (push value result)
          (when (eq key :bar)
            (push value result))))
      (nreverse result))))

(mic-ert-macroexpand-1 mic-defmic-adapter
  ((mic-test-mic-defmic-adapter feature-name
     :foo (123)
     :bar (456))
   . (parent-name feature-name
                  :hoge
                  (123 1 4 5)
                  :bar
                  (456 2 3)
                  (456 2 3)
                  :baz
                  (6 7))))

(mic-defmic mic-test-mic-defmic-inputter parent-name
  :filters '(mic-test-filter-const-1 mic-test-filter-const-2)
  :inputter
  (lambda (psudo-plist)
    "Take PSUDO-PLIST.
:foo is allowed to be append."
    (let (result key)
      (while psudo-plist
        (let ((now (pop psudo-plist)))
          (if (keywordp now)
              (setq key now)
            (if (eq key :foo)
                (mic-plist-put-append result key (list now))
              (mic-plist-put result key now)))))
      result)))

(mic-ert-macroexpand-1 mic-defmic-inputter
  ((mic-test-mic-defmic-inputter feature-name
     :foo 123
     :bar (456)
     :foo 789 101)
   . (parent-name feature-name
                  :foo
                  (123 789 101 1 4 5)
                  :bar
                  (456 2 3)
                  :baz
                  (6 7))))

(provide 'mic-test)
;;; mic-test.el ends here
