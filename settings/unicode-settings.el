;; -*- Emacs-Lisp -*-
;; Last modified: <2015-10-13 19:08:13 Tuesday by wongrichard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1
;; PUBLIC LICENSE: GPLv3

(when (string= system-type "gnu/linux")
  ;; ENV path correction for (Mac os x)
  (require 'ibus)
  (setq ibus-agent-file-name (concat plugins-path-r "ibus-el-agent")
        ibus-python-shell-command-name "python2.7"))
(provide 'unicode-settings)
;; unicode-settings ends here.
;;;
