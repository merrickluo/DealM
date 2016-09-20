;; -*- Emacs-Lisp -*-
;; Last modified: <2016-09-20 17:12:04 Tuesday by richard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1
;; PUBLIC LICENSE: GPLv3


(use-package ibus
  :if (string= system-type "gnu/linux")
  :init   ; execute code after a package is loaded
  (setq ibus-agent-file-name (concat plugins-path-r "ibus-el-agent")
        ibus-python-shell-command-name "python2.7"))


(provide 'unicode-settings)
;; unicode-settings ends here.
;;;
