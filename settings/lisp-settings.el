;; -*- Emacs-Lisp -*-
;; Last modified: <2013-04-26 17:26:24 Friday by richard>

;; Copyright (C) 2013 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1
;; PUBLIC LICENSE: GPLv3

;; Settings for clojure
;; -----------------------------------[Settings for clojure]
(add-to-list 'load-path (concat plugins-path-r "clojure-mode"))
(add-to-list 'load-path (concat plugins-path-r "nrepl"))
(add-to-list 'load-path (concat plugins-path-r "ac-nrepl"))

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

(require 'nrepl)
(require 'clojure-mode)

(autoload 'clojure-mode "clojure-mode" "" t)
(autoload 'ac-nrepl-setup "ac-nrepl" "" t)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)


(eval-after-load "ob"
  ;; clojure integration for emacs
  '(progn
     (defun clojure-for-org-mode()

       (defvar org-babel-default-header-args:clojure
         '((:results . "silent") (:tangle . "yes")))

       (defun org-babel-execute:clojure (body params)
         "Evaluate a block of Clojure code with Babel."
         (lisp-eval-string body)
         "Done!"))
     (clojure-for-org-mode)))


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
