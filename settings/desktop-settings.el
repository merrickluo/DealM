;; -*- Emacs-Lisp -*-
;; Last modified: <2012-07-28 11:50:34 Saturday by richard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1
;; PUBLIC LICENSE: GPLv3

;; (require 'desktop)
(desktop-save-mode 1)
;; desktop autosave
(setq desktop-save t
      ;; alway load desktop even when loacked
      desktop-load-locked-desktop t)

(desktop-read (concat emacs-root-path ""))
(provide 'desktop-settings)
;; desktop-settings ends here.
;;;
