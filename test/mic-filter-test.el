;;; mic-filter-test.el --- test for mic-filter

;; Copyright (C) 2022  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords:

;; Version: 0.0.0
;; Package-Requires:
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

;;; Code:

(require 'ert)

(require 'undercover)
(undercover "*.el"
            (:report-format 'codecov)
            (:report-file "coverage-final.json")
            (:send-report nil))

(require 'mic-filter)

(ert-deftest mic-filter-hook-quote ()
  (should (equal
           (mic-filter-hook-quote
            '(:hook ((after-init-hook . func1)
                     (switch-buffer-hook . (lambda () 1)))))
           '(:eval
             ((add-hook 'after-init-hook #'func1)
              (add-hook 'switch-buffer-hook #'(lambda nil 1)))))))



(ert-deftest mic-filter-straight ()
  (should (equal (mic-filter-straight '(:straight (x y (emacs-ja
                                                        :host github
                                                        :repo "ayatakesi/ayatakesi.github.io"
                                                        :files ("emacs/26.1/emacs-ja.info")))))
                 '(:eval-installation ((straight-use-package 'x)
                                       (straight-use-package 'y)
                                       (straight-use-package
                                        '(emacs-ja :host github :repo "ayatakesi/ayatakesi.github.io"
                                                   :files ("emacs/26.1/emacs-ja.info")))))))
  (should (eq (plist-get (mic-filter-straight '( :straight (x y (emacs-ja
                                                                 :host github
                                                                 :repo "ayatakesi/ayatakesi.github.io"
                                                                 :files ("emacs/26.1/emacs-ja.info")))
                                                 :foo bar))
                         :foo)
              'bar)))

(ert-deftest mic-filter-el-get ()
  (should (equal (mic-filter-el-get
                  '(:el-get (x y
                               (zenburn-theme
                                :url "https://raw.githubusercontent.com/bbatsov/zenburn-emacs/master/zenburn-theme.el"
                                (load-theme 'zenburn t)))))
                 '(:eval-installation ((el-get-bundle x)
                                       (el-get-bundle y)
                                       (el-get-bundle
                                         zenburn-theme
                                         :url "https://raw.githubusercontent.com/bbatsov/zenburn-emacs/master/zenburn-theme.el"
                                         (load-theme 'zenburn t))))))
  (should (eq (plist-get (mic-filter-el-get
                          '( :el-get (x y
                                        (zenburn-theme
                                         :url "https://raw.githubusercontent.com/bbatsov/zenburn-emacs/master/zenburn-theme.el"
                                         (load-theme 'zenburn t)))
                             :foo bar))
                         :foo)
              'bar)))



(ert-deftest mic-filter-define-key-general ()
  (should (equal (mic-filter-define-key-general
                  '(:define-key-general
                    ((keymap1
                      ("C-d" . #'func1)
                      ("C-q" . #'func2))
                     (override
                      ("C-a" . #'func3)
                      ("C-e" . #'func4)))))
                 '(:eval
                   ((general-define-key
                     :keymaps 'keymap1
                     "C-d" #'func1
                     "C-q" #'func2)
                    (general-define-key
                     :keymaps 'override
                     "C-a" #'func3
                     "C-e" #'func4))))))

(ert-deftest mic-filter-general-define-key ()
  (should (equal (mic-filter-general-define-key
                  '(:general-define-key
                    (( :keymaps 'keymap1
                       "C-d"  #'func1
                       "C-q"  #'func2)
                     ( :keymaps 'override
                       "C-a" #'func3
                       "C-e" #'func4))))
                 '(:eval
                   ((general-define-key
                     :keymaps 'keymap1
                     "C-d" #'func1
                     "C-q" #'func2)
                    (general-define-key
                     :keymaps 'override
                     "C-a" #'func3
                     "C-e" #'func4))))))



(ert-deftest mic-filter-hydra ()
  (should (equal (mic-filter-hydra
                  '(:hydra ((hydra-window-resizer
                             nil
                             ("p" shrink-window "shrink")
                             ("n" enlarge-window "enlarge")
                             ("f" enlarge-window-horizontally "enlarge-horizontally")
                             ("b" shrink-window-horizontally "shrink-horizontally")
                             ;; ("k" shrink-window)
                             ;; ("j" enlarge-window)
                             ;; ("l" enlarge-window-horizontally)
                             ;; ("h" shrink-window-horizontally)
                             ("<down>" shrink-window)
                             ("<up>" enlarge-window)
                             ("<right>" enlarge-window-horizontally)
                             ("<left>" shrink-window-horizontally)
                             ("q" nil "quit")))))
                 '(:eval
                   ((defhydra hydra-window-resizer nil
                      ("p" shrink-window "shrink")
                      ("n" enlarge-window "enlarge")
                      ("f" enlarge-window-horizontally "enlarge-horizontally")
                      ("b" shrink-window-horizontally "shrink-horizontally")
                      ("<down>" shrink-window)
                      ("<up>" enlarge-window)
                      ("<right>" enlarge-window-horizontally)
                      ("<left>" shrink-window-horizontally)
                      ("q" nil "quit")))))))

(ert-deftest mic-filter-mykie ()
  (should (equal (mic-filter-mykie
                  '(:mykie ((global-map ("C-w" :default hydra-window-resizer/body :region kill-region)))))
                 '(:eval ((mykie:define-key global-map "C-w"
                            :default hydra-window-resizer/body
                            :region kill-region))))))

(provide 'mic-filter-test)
;;; mic-filter-test.el ends here
