;; -*- Emacs-Lisp -*-
;; Last modified: <2016-09-23 00:03:59 Friday by richard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.3
;; PUBLIC LICENSE: GPLv3
;; The auto-insert-settings do *NOT* need use-package style writting

(setq auto-insert-query nil
      auto-insert-directory template-path-r)

(defun insert-snippet (snippet)
  "Insert snippet SNIPPET."
  (interactive "s")
  (insert snippet)
  (yas-expand))

(defun insert-headx-snippet ()
  "Insert headx snippet."
  (insert-snippet "headx"))

(defun insert-abbrev (abbrev-name)
  "Insert abbrev ABBREV-NAME"
  (interactive "s")
  (insert abbrev-name)
  (expand-abbrev))

;; C/C++ head file.
(define-auto-insert "\\.\\([Hh]\\|hh\\|hxx\\|hpp\\)$"
  '(lambda()
     (insert-snippet "headxh")))

;; Add plan Snippet.
(define-auto-insert "\\.\\(todo\\|do\\|plan\\)$"
  '(lambda()
     (insert-snippet "headxplan")))

;; Flex snippet.
(define-auto-insert "\\.l[l]?$"
  '(lambda()
     (insert-snippet "headxflex")))

(mapc
 (lambda (suffix)
   (define-auto-insert (concat "\\." suffix "$") 'insert-headx-snippet))
 '("[Cc][Pp][Pp]"      ;; C++
   "[Cc][Xx][Xx]"      ;; C++
   "[Tt]?[Cc][Cc]"     ;; C++
   "i"                 ;; C++
   "js"                ;; js
   "CMakeLists.txt"    ;; js
   "makefile"          ;; makefile
   "headx"             ;; snippet
   "y[s]?"             ;; yassnippet
   "yy"                ;; bison
   "[Yy][Aa]?[Mm][Ll]" ;; YAML
   "el"                ;; elisp
   "sh"                ;; shell
   "org"               ;; org
   "pl"
   "py"                ;; python
   "htm\\(l\\)?"))

(auto-insert-mode 1)

(provide 'auto-insert-settings)
;; auto-insert-settings ends here.
;;;
