;; -*- Emacs-Lisp -*-
;; Last modified: <2012-07-29 22:53:02 Sunday by richard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1
;; PUBLIC LICENSE: GPLv3

(require 'dired+)
(require 'dired-details+)
(require 'dired-details)

(autoload 'openwith-mode "openwith" t)
(openwith-mode t)

(push  '("\\.pdf\\'" "evince" (file)) openwith-associations)
(push  '("\\.epub\\'" "calibre" (file)) openwith-associations)
(push  '("\\.rar\\'" "peazip" (file)) openwith-associations)

(define-key dired-mode-map "1" 'delete-other-windows)
(define-key dired-mode-map "2" 'split-window-vertically)
(define-key dired-mode-map "3" 'split-window-horizontally)
(define-key dired-mode-map "o" 'other-window)


(provide 'dired-settings)
;; dired-settings ends here.
;;;