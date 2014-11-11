;; -*- Emacs-Lisp -*-
;; Last modified: <2014-11-11 14:24:06 Tuesday by wongrichard>

;; Copyright (C) 2012-2013 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1
;; PUBLIC LICENSE: GPLv3

(require 'dired+)
(define-key dired-mode-map "\C-o"    'dired-display-file)         ; `C-o'

(add-hook 'dired-mode-hook '(lambda ()
                              (require 'dired-x)
                              (dired-omit-mode t)))

(setq dired-dwim-target t)

(autoload 'openwith-mode "openwith" t)

(defun dired-key-bindings ()
  (openwith-mode t)

  (if (string= system-type "darwin")
      (progn
        (push '("\\.pdf\\'" "open" (file)) openwith-associations))
    (progn
      (push '("\\.pdf\\'" "evince" (file)) openwith-associations)
      (push '("\\.epub\\'" "calibre" (file)) openwith-associations)
      (push '("\\.rar\\'" "peazip" (file)) openwith-associations)))
  (define-key dired-mode-map "1" 'delete-other-windows)
  (define-key dired-mode-map "2" 'split-window-vertically)
  (define-key dired-mode-map "3" 'split-window-horizontally)
  (define-key dired-mode-map "o" 'other-window)
  ;; From color-moccur
  (define-key dired-mode-map "O" 'dired-do-moccur)
  (defvar dired-sort-map (make-sparse-keymap))

  (define-key dired-mode-map "s" dired-sort-map)

  (define-key dired-sort-map "s" (lambda () "sort by Size" (interactive) (dired-sort-other (concat dired-listing-switches " -S"))))
  (define-key dired-sort-map "x" (lambda () "sort by eXtension" (interactive) (dired-sort-other (concat dired-listing-switches " -X"))))
  (define-key dired-sort-map "t" (lambda () "sort by Time" (interactive) (dired-sort-other (concat dired-listing-switches " -t"))))
  (define-key dired-sort-map "n" (lambda () "sort by Name" (interactive) (dired-sort-other dired-listing-switches)))
  (define-key dired-sort-map "?" (lambda () "sort help" (interactive) (message "s Size; x eXtension; t Time; n Name")))
  (define-key dired-sort-map "h" (lambda () "sort help" (interactive) (message "s Size; x eXtension; t Time; n Name")))
  (define-key dired-sort-map "/" (lambda () "sort help" (interactive) (message "s Size; x eXtension; t Time; n Name"))))

(eval-after-load "dired"
  '(dired-key-bindings))

(provide 'dired-settings)
;; dired-settings ends here.
;;;
