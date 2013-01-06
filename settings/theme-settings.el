;; -*- Emacs-Lisp -*-
;; Last modified: <2013-01-05 16:17:59 Saturday by richard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1
;; PUBLIC LICENSE: FreeBSD
(add-to-list 'custom-theme-load-path (concat plugins-path-r "emacs-color-theme-solarized/"))

(load-theme 'wheatgrass t)

(autoload 'hl-line-face "hl-line+")
(eval-after-load "hl-line"
  '(set-face-background hl-line-face "grey20")
)
;; hl-line-face

(provide 'theme-settings)
;; theme-settings ends here.
;;;
