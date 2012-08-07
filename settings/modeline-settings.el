;; -*- Emacs-Lisp -*-
;; Last modified: <2012-08-07 09:02:57 Tuesday by richard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1
;; PUBLIC LICENSE: GPLv3

(autoload 'sml/setup "smart-mode-line" "\
Setup the mode-line, or revert it.

If argument is a non-positive integer, revert any changes made.
Otherwise, setup the mode-line.

\(fn &optional ARG)" t nil)
(setq sml/hidden-modes "*")
(sml/setup)

(provide 'modeline-settings)
;; modeline-settings ends here.
;;;
