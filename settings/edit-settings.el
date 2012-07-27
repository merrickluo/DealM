;; -*- Emacs-Lisp -*-
;; Last modified: <2012-07-27 23:28:59 Friday by richard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.8
;; PUBLIC LICENSE: GPLv3

(require 'edit-functions)
(require 'browse-kill-ring+)


;; global keys
;; ------------------------------------------------------------------

(global-set-key (kbd "C-x q") 'switch-major-mode)
(global-set-key (kbd "C-x m") 'get-mode-name)

(global-set-key (kbd "C-o")     'open-line-if-active-delete-then-open-line)
(global-set-key (kbd "C-c M-c") 'copy-file-name)
(global-set-key (kbd "C-x M-O") 'open-current-file-with-app)
(global-set-key (kbd "C-x s")   'switch-to-scratch)
(global-set-key (kbd "C-x M-s") 'switch-to-shell)
(global-set-key (kbd "C-x M-m") 'switch-to-message)
(global-set-key (kbd "M-SPC")   'just-one-space)
(global-set-key (kbd "C-x j")   'jump-to-register)   ;; global jump
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two
(global-set-key (kbd "C-c q") 'join-line)
;; using regular expression as default search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

(global-set-key (kbd "M-C-k")        'kill-whole-paragraph)
(global-set-key (kbd "M-C-y")        'browse-kill-ring)
(global-set-key (kbd "M-w")          'smart-copy)
(global-set-key (kbd "C-z")          'undo)
(global-set-key (kbd "M-k")          'kill-paragraph)
(global-set-key (kbd "M-C")          'copy-whole-paragraph)
(global-set-key (kbd "C-M-w")        'smart-insert-line)
(global-set-key (kbd "C-k")          'smart-kill)
(global-set-key (kbd "C-a")          'smart-move-beginning-of-line)
(global-set-key (kbd "C-\\")         'delete-indentation)
(global-set-key (kbd "C-x M-M")      'mark-invisible-region)
(global-set-key (kbd "M-U")          'del-to-begin)
(global-set-key (kbd "C-^")          'case-trans)
(global-set-key (kbd "C-6")          'case-trans)
(global-set-key (kbd "C-w")          'backward-kill-word-or-kill-region)
(global-set-key (kbd "C-x S")        'mark-whole-sexp)
(global-set-key (kbd "C-x C-k")      'kill-whole-sexp)
(global-set-key (kbd "C-x w")        'copy-sexp)
(global-set-key (kbd "C-x M-w")      'copy-sexp)
(global-set-key (kbd "C-x TAB")      'smart-indent)
(global-set-key (kbd "C-h")          'c-electric-backspace-kill)
(global-set-key (kbd "M-Y")          'redo)
(global-set-key (kbd "M-q")          'fill-paragraph-justify)
(global-set-key (kbd "<escape> SPC") 'just-one-space)

(if window-system
    (global-set-key (kbd "C-z")      'undo))


;; registers.
;; ------------------------------------------------------------------
(set-register ?e '(file . "~/.emacs"))               ;; C-x r j e: register jump to the .emacs(home).
(set-register ?w '(file . "~/.words.org"))           ;; C-x r j w: register jump to words.
(set-register ?t '(file . "/tmp/test.py"))           ;; C-x r j t: goto test python file.




;; smart-operator
;; ------------------------------------------------------------------
;;;### (autoloads (smart-operator-self-insert-command smart-operator-mode-on
;;;;;;  smart-operator-mode) "smart-operator" "smart-operator.el"
;;;;;;  (20476 13650))
;;; Generated autoloads from smart-operator.el

(autoload 'smart-operator-mode "smart-operator" "\
Insert operators with surrounding spaces smartly.

\(fn &optional ARG)" t nil)

(autoload 'smart-operator-mode-on "smart-operator" "\
Turn on `smart-operator-mode'.

\(fn)" nil nil)

(autoload 'smart-operator-self-insert-command "smart-operator" "\
Insert the entered operator plus surrounding spaces.

\(fn ARG)" t nil)

;; (smart-operator-mode-on)

(provide 'edit-settings)
;; edit-settings ends here.
;;;
