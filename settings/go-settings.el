;; -*- Emacs-Lisp -*-
;; Last modified: <2017-01-05 17:31:37 Thursday by merrick>

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
  :defer t
  :after (go-eldoc go-autocomplete)
  :commands (gofmt gofmt-before-save)
  :mode "\\.go\\'"
  :init
  (use-package go-eldoc)
  (use-package go-autocomplete)
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (add-hook 'go-mode-hook 'go-set-shortcut)
  (add-hook 'go-mode-hook 'auto-complete-mode)
  (add-hook 'go-mode-hook 'linum-mode)
  (add-hook 'go-mode-hook 'yas-minor-mode)
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package protobuf-mode
  :defer t
  :mode "\\.proto\\'")

(provide 'go-settings)
;; go-settings ends here.
;;;
