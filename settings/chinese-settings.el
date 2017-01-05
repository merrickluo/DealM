;; -*- Emacs-Lisp -*-
;; Last modified: <2017-01-05 14:51:52 Thursday by merrick>

;; Copyright (C) 2017 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1
;; PUBLIC LICENSE: GPLv3

(add-to-list 'load-path (concat plugins-path-r "fcitx"))
(add-to-list 'load-path (concat plugins-path-r "pangu-spacing"))

(use-package fcitx
  :config
  (fcitx-aggressive-setup))

(use-package pangu-spacing
  :config
  (global-pangu-spacing-mode t))

(provide 'chinese-settings)
;; chinese-settings ends here
;;;
