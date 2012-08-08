;; -*- Emacs-Lisp -*-
;; Last modified: <2012-08-08 13:27:54 Wednesday by richard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1
;; PUBLIC LICENSE: GPLv3
;; include cedet and cedet's plugins settings.

(require 'cedet)
(add-to-list 'load-path (concat plugins-path-r "ecb/"))
(require 'ecb)
(provide 'cedet-settings)
;; cedet-settings ends here.
;;;
