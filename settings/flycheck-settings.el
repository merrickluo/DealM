;; -*- Emacs-Lisp -*-
;; Last modified: <2016-08-27 10:27:56 Saturday by wongrichard>

;; Copyright (C) 2016 Richard Wong

;; Author: Richard Wong
;; Email: github@cccc.im

;; Version: 0.1
;; PUBLIC LICENSE: GPLv3

;;; code:
(add-to-list 'load-path (concat plugins-path-r "flycheck/"))
(add-to-list 'load-path (concat plugins-path-r "seq.el"))

(use-package
  flycheck
  :init                                 ; before load package
  (setq flycheck-mode-line-prefix "F"))

(provide 'flycheck-settings)
;; flycheck-settings ends here.
;;;
