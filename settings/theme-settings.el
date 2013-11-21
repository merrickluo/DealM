;; -*- Emacs-Lisp -*-
;; Last modified: <2013-10-07 17:02:54 Monday by wongrichard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1
;; PUBLIC LICENSE: FreeBSD
(add-to-list 'custom-theme-load-path (concat plugins-path-r "emacs-color-theme-solarized/"))

(load-theme 'tomorrow-night t)

(autoload 'hl-line-face "hl-line+")
(eval-after-load "hl-line"
  '(set-face-background hl-line-face "grey20"))
;; hl-line-face

(provide 'theme-settings)
;; theme-settings ends here.
;;;
