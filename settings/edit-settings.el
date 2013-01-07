;; -*- Emacs-Lisp -*-
;; Last modified: <2013-01-07 19:58:13 Monday by richard>

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


;; back-button settings
;; -----------------------------------[back-button settings]
(autoload 'back-button-global-backward            "back-button" "" t)
(autoload 'back-button-global-forward             "back-button" "" t)
(autoload 'back-button-local-forward              "back-button" "" t)
(autoload 'back-button-local-backward             "back-button" "" t)
(autoload 'back-button-push-mark-local-and-global "back-button" "" t)
(autoload 'back-button-mode                       "back-button" )
(back-button-mode t)


;; occur
;; -------------------------------------------------[ioccur]

;; using regular expression as default search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

(define-key isearch-mode-map (kbd "C-S-O")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (multi-occur-in-this-mode
       (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

(eval-after-load "projectile"
  (define-key isearch-mode-map (kbd "C-S-O")
    (lambda () (interactive)
      (let ((case-fold-search isearch-case-fold-search)
            (search-string (if isearch-regexp isearch-string (regexp-quote isearch-string))))
        (if (projectile-project-p)
            (projectile-multi-occur search-string)
          (multi-occur-in-this-mode search-string))))))

;; other global keys.
(global-set-key  "\C-xc"             'org-capture)
(global-set-key (kbd "M-C-k")        'kill-whole-paragraph)
(global-set-key (kbd "M-C-y")        'browse-kill-ring)
(when (string= system-type "windows-nt")
  (w32-register-hot-key (kbd "M-w")))
(global-set-key (kbd "M-w")          'smart-copy)
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

;; pager: Fix windows lines bugs in scroll-up and down.
(require 'pager)
(global-set-key "\C-v"	   'pager-page-down)
(global-set-key [next] 	   'pager-page-down)
(global-set-key "\ev"	   'pager-page-up)
(global-set-key [prior]	   'pager-page-up)
(global-set-key '[M-up]    'pager-row-up)
(global-set-key '[M-kp-8]  'pager-row-up)
(global-set-key '[M-down]  'pager-row-down)
(global-set-key '[M-kp-2]  'pager-row-down)

(global-set-key [(shift home)] '(lambda () (interactive) (other-window -1)))
(global-set-key [(shift end)]  '(lambda () (interactive) (other-window 1)))

(global-set-key '[kp-home]  'beginning-of-buffer) ; [Home]
(global-set-key '[home]     'beginning-of-buffer) ; [Home]
(global-set-key '[kp-end]   'end-of-buffer)       ; [End]
(global-set-key '[end]      'end-of-buffer)       ; [End]

(if window-system
    (global-set-key (kbd "C-z")      'undo))

;; replace global keys
(global-set-key (kbd "M-r")     'query-replace-regexp)
(global-set-key (kbd "C-x ?")   'help-command)
(global-set-key (kbd "C-x /")   'help-command)


(global-set-key (kbd "C-x u")
                '(lambda ()
                   "Revert buffer without confirmation."
                   (interactive)
                   (revert-buffer t t)))
(global-set-key (kbd "C-x M-K")  'revert-buffer-with-gbk)
(global-set-key (kbd "C-x U")    'revert-buffer-with-coding-system-no-confirm)

;; Fastnav
;; ------------------------------------------------------------------
(autoload 'fastnav-sprint-forward "fastnav" "\
Performs a sequence of jumping forward to the next character
matching the keyboard event.

\(fn ARG)" t nil)

(autoload 'fastnav-sprint-backward "fastnav" "\
Performs a sequence of jumping backward to the next character
matching the keyboard event.

\(fn ARG)" t nil)

(global-set-key "\M-S" 'fastnav-sprint-backward)
;; an isearch style.
(global-set-key "\M-s" 'fastnav-sprint-forward)


;; goto settings.
;; ------------------------------------------------------------------

(def-position-command goto-plugins
  (kbd "C-x g p") plugins-path-r)
(def-position-command goto-settings
  (kbd "C-x g s") settings-path-r)
(def-position-command goto-emacs-root
  (kbd "C-x g e") emacs-root-path)
(def-position-command goto-tmp
  (kbd "C-x g t") "/tmp")

(global-set-key (kbd "C-]")     'goto-paren)
(global-set-key (kbd "C-M-]")   'ywb-indent-accoding-to-paren)

;; align settings.
(global-set-key (kbd "C-x a")   'align-current)
(global-set-key (kbd "C-x M-a") 'align-regexp)

;; hungry delete
;; ------------------------------------------------------------------
(autoload 'turn-on-hungry-delete-mode "hungry-delete")
(define-globalized-minor-mode global-hungry-delete-mode hungry-delete-mode turn-on-hungry-delete-mode)
(global-hungry-delete-mode t)


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

;; mc-edit-lines settings
;; ----------------------------------------[mc-edit-lines settings]
(add-to-list 'load-path (concat plugins-path-r "multiple-cursors"))
(autoload 'mc/edit-lines "mc-edit-lines" "" t)
(global-set-key (kbd "C-x \\") 'mc/edit-lines)
(global-set-key (kbd "C-x |")  'mc/edit-lines)

;; moccur settings
;; ----------------------------------------[moccur settings]
(setq *moccur-buffer-name-exclusion-list*
      '(".+TAGS.+" "*Completions*" "*magit-process*" "*Messages*"
        ))
(setq moccur-split-word t)
(setq dmoccur-use-list t)
(setq dmoccur-use-project t)
(setq dmoccur-list
      '(
        ("dir" default-directory (".*") dir)
        ("config" emacs-root-path  ("\\.py$" "\\.el$") nil)
        ))

(global-set-key "\C-c\C-o" 'search-buffers)

(provide 'edit-settings)
;; edit-settings ends here.
;;;
