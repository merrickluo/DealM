;; -*- Emacs-Lisp -*-
;; Last modified: <2016-09-22 23:07:17 Thursday by richard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.2
;; PUBLIC LICENSE: GPLv3

(add-to-list 'load-path (concat plugins-path-r "auto-complete/"))
(add-to-list 'load-path (concat plugins-path-r "popup/"))

(use-package auto-complete
  :commands
  (auto-complete auto-complete-mode global-auto-complete-mode)
  :init
  (setq ac-dwim t
        ac-auto-start 3
        ;; ac-auto-show-menu t
        ;; ac-disable-faces nil
        ;; ac-candidate-limit ac-menu-height
        ac-quick-help-delay .5))

(use-package auto-complete-config
  :commands (ac-config-default))

;; Disabling Yasnippet completion
;; TODO:
;; https://github.com/gabrielelanaro/emacs-for-python/blob/master/epy-completion.el
;; reintegrate snippet into ac-compelte with epy-snips-from-table

(provide 'auto-complete-setting)
;; auto-complete-setting ends here.
;;;
