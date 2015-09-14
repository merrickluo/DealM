;; -*- Emacs-Lisp -*-
;; Last modified: <2015-09-14 14:22:36 Monday by wongrichard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1
;; PUBLIC LICENSE: FreeBSD

(add-to-list 'custom-theme-load-path themes-path-r)

(load-theme 'monokai t)

(autoload 'hl-line-face "hl-line+")
(eval-after-load "hl-line"
  '(set-face-background hl-line-face "grey20"))
;; hl-line-face

(provide 'theme-settings)
;; theme-settings ends here.
;;;
