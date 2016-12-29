;; -*- Emacs-Lisp -*-
;; Last modified: <2016-12-29 17:49:24 Thursday by merrick>

;; Copyright (C) 2016 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1
;; PUBLIC LICENSE: GPLv3

(add-to-list 'load-path (concat plugins-path-r "go-mode"))
(add-to-list 'load-path (concat plugins-path-r "go-eldoc"))

(defun go-set-shortcut ()
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark))

(use-package go-mode
  :after (go-eldoc go-autocomplete)
  :commands (gofmt gofmt-before-save)
  :mode "\\.go\\'"
  :init
  (add-hook 'go-mode-hook 'go-set-shortcut)
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package go-eldoc
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-autocomplete
  :init
  (add-hook 'go-mode-hook 'auto-complete-mode)
  (add-hook 'go-mode-hook 'yas-minor-mode))

(use-package protobuf-mode
  :mode "\\.proto\\'")

(provide 'go-settings)
;; go-settings ends here.
;;;
