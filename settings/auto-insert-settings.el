;; -*- Emacs-Lisp -*-
;; Last modified: <2013-03-26 17:33:11 Tuesday by richard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.2
;; PUBLIC LICENSE: GPLv3

(auto-insert-mode 1)

(defun auto-insert-settings ()
  "Settings for `auto-insert'."
  (setq auto-insert-query nil)
  (setq auto-insert-directory template-path-r)
  ;; CMakeLists.txt (CMake file)
  (define-auto-insert "CMakeLists.txt$"                 '(lambda()
                                                           (insert-snippet "headx")))
  ;; C/C++ head file.
  (define-auto-insert "\\.\\([Hh]\\|hh\\|hxx\\|hpp\\)$" '(lambda()
                                                           (insert-snippet "headxh")))
  ;; Add plan Snippet.
  (define-auto-insert "\\.\\(todo\\|do\\|plan\\)$"      '(lambda()
                                                           (insert-snippet "headxplan")))
  ;; Flex snippet.
  (define-auto-insert "\\.l[l]?$"                       '(lambda()
                                                           (insert-snippet "headxflex")))

  (defun insert-headx-snippet ()
    "Insert headx snippet."
    (insert-snippet "headx"))

  (defun insert-abbrev (abbrev-name)
    "Insert abbrev ABBREV-NAME"
    (interactive "s")
    (insert abbrev-name)
    (expand-abbrev))

  (defun insert-snippet (snippet)
    "Insert snippet SNIPPET."
    (interactive "s")
    (insert snippet)
    (yas-expand))

  (mapc
   (lambda (suffix)
     (define-auto-insert (concat "\\." suffix "$") 'insert-headx-snippet))
   '("[Cc][Pp][Pp]"                ;; C++
     "[Cc][Xx][Xx]"                ;; C++
     "[Tt]?[Cc][Cc]"               ;; C++
     "i"                           ;; C++
     "makefile"                    ;; makefile
     "headx"                       ;; snippet
     "y[s]?"                       ;; yassnippet
     "yy"                          ;; bison
     "el"                          ;; elisp
     "sh"                          ;; shell
     "org"                         ;; org
     "pl"
     "py"                          ;; python
     "htm\\(l\\)?")))              ;;html

(eval-after-load "autoinsert"
  `(auto-insert-settings))

(provide 'auto-insert-settings)
;; auto-insert-settings ends here.
;;;
