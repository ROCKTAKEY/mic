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

(defmacro mic-ert-macroexpand-1 (name &rest args)
  "Define test named NAME.
The test compare macro expandation of `car' of each element of ARGS with `cdr' of it."
  (declare (indent defun))
  `(ert-deftest ,name ()
     ,@(mapcar
        (lambda (arg)
          `(should (equal (macroexpand-1 ',(car arg))
                          ',(cdr arg))))
        args)))

(mic-ert-macroexpand-1 mic-autoload-interactive
  ((mic package-name
     :autoload-intaractive
     (find-file
      write-file))
   . (prog1 'package-name
       (autoload #'find-file "package-name" nil t)
       (autoload #'write-file "package-name" nil t))))

(mic-ert-macroexpand-1 mic-autoload-noninteractive
  ((mic package-name
     :autoload-nonintaractive
     (cl-map
      cl-mapcar))
   . (prog1 'package-name
       (autoload #'cl-map "package-name")
       (autoload #'cl-mapcar "package-name"))))

(mic-ert-macroexpand-1 mic-custom
  ((mic package-name
     :custom
     ((a . 1)
      (b . (+ 1 2))))
   . (prog1 'package-name
       (customize-set-variable 'a 1)
       (customize-set-variable 'b
                               (+ 1 2)))))

(mic-ert-macroexpand-1 mic-custom-after-load
  ((mic package-name
     :custom-after-load
     ((a . 1)
      (b . (+ 1 2))))
   . (prog1 'package-name
       (with-eval-after-load 'package-name
         (customize-set-variable 'a 1)
         (customize-set-variable 'b
                                 (+ 1 2))))))

(mic-ert-macroexpand-1 mic-eval
  ((mic package-name
     :eval
     ((message "Hello")
      (message "World")))
   . (prog1 'package-name
       (message "Hello")
       (message "World"))))

(mic-ert-macroexpand-1 mic-eval-after-load
  ((mic package-name
     :eval-after-load
     ((message "Hello")
      (message "World")))
   . (prog1 'package-name
       (with-eval-after-load 'package-name
         (message "Hello")
         (message "World")))))

(mic-ert-macroexpand-1 mic-define-key
  ((mic package-name
     :define-key
     ((global-map
       ("C-t" . #'other-window)
       ("C-n" . #'next-window))
      (prog-mode-map
       ("M-a" . #'beginning-of-buffer)
       ("M-e" . #'end-of-buffer))))
   . (prog1 'package-name
       (define-key global-map (kbd "C-t") #'other-window)
       (define-key global-map (kbd "C-n") #'next-window)
       (define-key prog-mode-map (kbd "M-a") #'beginning-of-buffer)
       (define-key prog-mode-map (kbd "M-e") #'end-of-buffer))))

(mic-ert-macroexpand-1 mic-define-key-after-load
  ((mic package-name
     :define-key-after-load
     ((c-mode-map
       ("C-t" . #'other-window)
       ("C-n" . #'next-window))
      (c++-mode-map
       ("M-a" . #'beginning-of-buffer)
       ("M-e" . #'end-of-buffer))))
   . (prog1 'package-name
       (with-eval-after-load 'package-name
         (define-key c-mode-map (kbd "C-t") #'other-window)
         (define-key c-mode-map (kbd "C-n") #'next-window)
         (define-key c++-mode-map (kbd "M-a") #'beginning-of-buffer)
         (define-key c++-mode-map (kbd "M-e") #'end-of-buffer)))))

(mic-ert-macroexpand-1 mic-hook
  ((mic package-name
     :hook
     ((after-init-hook . #'ignore)
      (prog-mode-hook . (lambda ()))))
   . (prog1 'package-name
       (add-hook 'after-init-hook #'ignore)
       (add-hook 'prog-mode-hook (lambda ())))))

(provide 'mic-test)
;;; mic-test.el ends here
