;; -*- Emacs-Lisp -*-
;; Last modified: <2015-09-26 22:20:08 Saturday by wongrichard>

;; Copyright (C) 2012-2015 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1
;; PUBLIC LICENSE: FreeBSD

(add-to-list 'custom-theme-load-path themes-path-r)

(load-theme default-theme-r t)

(autoload 'hl-line-face "hl-line+")
(eval-after-load "hl-line"
  '(set-face-background hl-line-face "grey20"))
;; hl-line-face

(provide 'theme-settings)
;; theme-settings ends here.
;;;
