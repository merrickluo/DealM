;; -*- Emacs-Lisp -*-
;; Last modified: <2012-11-20 15:09:02 Tuesday by richard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1
;; PUBLIC LICENSE: GPLv3

(require 'dired+)
(require 'dired-details+)

(setq dired-dwim-target t)

(autoload 'openwith-mode "openwith" t)
(openwith-mode t)

(push  '("\\.pdf\\'" "evince" (file)) openwith-associations)
(push  '("\\.epub\\'" "calibre" (file)) openwith-associations)
(push  '("\\.rar\\'" "peazip" (file)) openwith-associations)

(define-key dired-mode-map "1" 'delete-other-windows)
(define-key dired-mode-map "2" 'split-window-vertically)
(define-key dired-mode-map "3" 'split-window-horizontally)
(define-key dired-mode-map "o" 'other-window)
;; From color-moccur
(define-key dired-mode-map "O" 'dired-do-moccur)

(defvar dired-sort-map (make-sparse-keymap))

(define-key dired-mode-map "s" dired-sort-map)

(define-key dired-sort-map "s" (lambda () "sort by Size" (interactive) (dired-sort-other (concat dired-listing-switches " -S"))))
(define-key dired-sort-map "x" (lambda () "sort by eXtension" (interactive) (dired-sort-other (concat dired-listing-switches " -X"))))
(define-key dired-sort-map "t" (lambda () "sort by Time" (interactive) (dired-sort-other (concat dired-listing-switches " -t"))))
(define-key dired-sort-map "n" (lambda () "sort by Name" (interactive) (dired-sort-other dired-listing-switches)))
(define-key dired-sort-map "?" (lambda () "sort help" (interactive) (message "s Size; x eXtension; t Time; n Name")))
(define-key dired-sort-map "h" (lambda () "sort help" (interactive) (message "s Size; x eXtension; t Time; n Name")))
(define-key dired-sort-map "/" (lambda () "sort help" (interactive) (message "s Size; x eXtension; t Time; n Name")))



(provide 'dired-settings)
;; dired-settings ends here.
;;;
