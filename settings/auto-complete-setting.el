;; -*- Emacs-Lisp -*-
;; Last modified: <2012-08-13 08:50:38 Monday by richard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.2
;; ideas from epy project
;; PUBLIC LICENSE: GPLv3
(add-to-list 'load-path (concat plugins-path-r "auto-complete/"))
(add-to-list 'load-path (concat plugins-path-r "popup/"))

(require 'auto-complete-config nil t)
(add-to-list 'ac-dictionary-directories
             (concat plugins-path-r "auto-complete/dict/"))

;; Do What I Mean mode
(setq ac-dwim t
      ac-auto-start 3
      ;; ac-auto-show-menu t
      ;; ac-disable-faces nil
      ;; ac-candidate-limit ac-menu-height
      ac-quick-help-delay .5)

(ac-config-default)


;; custom keybindings to use tab, enter and up and down arrows
(define-key ac-complete-mode-map (kbd "<tab>") 'ac-expand)
(define-key ac-complete-mode-map "\r" 'ac-complete)
(define-key ac-complete-mode-map "\M-n" 'ac-next)
(define-key ac-complete-mode-map "\M-p" 'ac-previous)


;; Disabling Yasnippet completion
(defun epy-snips-from-table (table)
  (with-no-warnings
    (let ((hashtab (ac-yasnippet-table-hash table))
          (parent (ac-yasnippet-table-parent table))
          candidates)
      (maphash (lambda (key value)
                 (push key candidates))
               hashtab)
      (identity candidates)
      )))

(defun epy-get-all-snips ()
  (require 'yasnippet) ;; FIXME: find a way to conditionally load it
  (let (candidates)
    (maphash
     (lambda (kk vv) (push (epy-snips-from-table vv) candidates)) yas/tables)
    (apply 'append candidates))
  )

;;(setq ac-ignores (concatenate 'list ac-ignores (epy-get-all-snips)))



;; python part
;; ------------------------------------------------------------------
;; ropemacs Integration with auto-completion
(defun ac-ropemacs-candidates ()
  (mapcar (lambda (completion)
            (concat ac-prefix completion))
          (rope-completions)))

(ac-define-source nropemacs
  '((candidates . ac-ropemacs-candidates)
    (symbol     . "p")))

(ac-define-source nropemacs-dot
  '((candidates . ac-ropemacs-candidates)
    (symbol     . "p")
    (prefix     . c-dot)
    (requires   . 0)))

(defun ac-nropemacs-setup ()
  (setq ac-sources (append '(ac-source-nropemacs
                             ac-source-nropemacs-dot) ac-sources)))
(defun ac-python-mode-setup ()
  (add-to-list 'ac-sources 'ac-source-yasnippet))

(add-hook 'python-mode-hook 'ac-python-mode-setup)
(add-hook 'rope-open-project-hook 'ac-nropemacs-setup)


(provide 'auto-complete-setting)
;; auto-complete-setting ends here.
;;;
