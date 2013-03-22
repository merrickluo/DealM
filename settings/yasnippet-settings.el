;; -*- Emacs-Lisp -*-
;; Copyright (C) 2012 Richard Wong

;; Last modified: <2013-03-22 09:01:57 Friday by richard>

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1c
;; PUBLIC LICENSE: GPLv3

(add-to-list 'load-path (concat plugins-path-r "yasnippet/"))
(require 'yasnippet)

(setq yas-snippet-dirs (concat emacs-root-path "snippets/"))

(yas-load-directory yas-snippet-dirs)
(yas-global-mode 1)

(global-set-key (kbd "\t")                     'yas-next-field-or-maybe-expand)
(global-set-key (kbd "M-k")                    'yas-prev-field)
(define-key yas-minor-mode-map (kbd "C-c C-f") 'yas-find-snippets)

(provide 'yasnippet-settings)
;; yasnippet-settings ends here.
;;;
