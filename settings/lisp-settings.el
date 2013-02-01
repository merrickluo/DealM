;; -*- Emacs-Lisp -*-
;; Last modified: <2013-02-01 14:43:24 Friday by richard>

;; Copyright (C) 2013 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1
;; PUBLIC LICENSE: GPLv3

;; Settings for clojure
;; -----------------------------------[Settings for clojure]
(add-to-list 'load-path (concat plugins-path-r "clojure-mode"))

(autoload 'clojure-mode "clojure-mode" "" t)
(autoload 'clojurescript-mode "clojurescript-mode" "" t)

;; nrepl path plugin for clojure
(add-to-list 'load-path (concat plugins-path-r "nrepl"))

(eval-after-load "clojure-mode"
  '(progn
     (require 'nrepl)
     (autoload 'nrepl "nrepl" "" t)
     (autoload 'ac-nrepl-setup "ac-nrepl" "DOCSTRING" t)
     (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
     (add-hook 'clojure-nrepl-mode-hook 'ac-nrepl-setup)))

(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'nrepl-mode)))

;; paredit
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)

(eval-after-load 'paredit
  ;; need a binding that works in the terminal
  '(progn
     (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "M-(") 'paredit-backward-slurp-sexp)))


(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'clojure-mode-hook          (lambda () (paredit-mode +1)))

;; Settings for elisp
;; -------------------------------------[Settings for elisp]
;; 1. Add eldoc for emacs
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(eval-after-load "eldoc"
  '(eldoc-add-command
    'paredit-backward-delete
    'paredit-close-round))


(provide 'lisp-settings)
;; lisp-settings ends here.
;;;
