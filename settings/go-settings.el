;; -*- Emacs-Lisp -*-
;; Last modified: <2016-12-28 16:41:54 Wednesday by merrick>

;; Copyright (C) 2016 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1
;; PUBLIC LICENSE: GPLv3

(add-to-list 'load-path (concat plugins-path-r "go-mode"))
(add-to-list 'load-path (concat plugins-path-r "go-eldoc"))

(use-package go-mode
  :after (go-eldoc go-autocomplete)
  :commands (gofmt gofmt-before-save)
  :mode "\\.go\\'"
  :init
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package go-eldoc
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-autocomplete
  :init
  (add-hook 'go-mode-hook 'auto-complete-mode)
  (add-hook 'go-mode-hook 'yas-minor-mode))

(provide 'go-settings)
;; go-settings ends here.
;;;
