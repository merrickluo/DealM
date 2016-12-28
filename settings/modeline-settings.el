;; -*- Emacs-Lisp -*-
;; Last modified: <2012-11-29 11:12:26 Thursday by richard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.2
;; PUBLIC LICENSE: GPLv3
(add-to-list 'load-path (concat plugins-path-r "smart-mode-line"))
(add-to-list 'load-path (concat plugins-path-r "rich-minority"))

(autoload 'sml/setup "smart-mode-line" "\
Setup the mode-line, or revert it.

If argument is a non-positive integer, revert any changes made.
Otherwise, setup the mode-line.

\(fn &optional ARG)" t nil)
;; (setq sml/hidden-modes "*")

(eval-after-load "smart-mode-line"
  '(let ((root-path-brief (abbreviate-file-name emacs-root-path))
          (settings-path-brief (abbreviate-file-name settings-path-r))
          (plugins-path-brief (abbreviate-file-name plugins-path-r)))

     ;; Added in the right order, they even work sequentially:
     (add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/" ":DB:"))
     (add-to-list 'sml/replacer-regexp-list '("^:DB:Documents" ":DDocs:"))
     (add-to-list 'sml/replacer-regexp-list
                  (cons (concat "^" root-path-brief) '(":EMACS:")))
     (add-to-list 'sml/replacer-regexp-list
                  (cons (concat "^" settings-path-brief) '(":ESET:")))
     (add-to-list 'sml/replacer-regexp-list
                  (cons (concat "^"  plugins-path-brief) '(":EADDON:")))
     (add-to-list 'sml/replacer-regexp-list '(" Flymake " " FM "))

     (setq sml/shorten-directory nil
           sml/hidden-modes '(" hl-p" " pair" " yas" " back" " ||"
                              " Abbrev" " Rbow" " _\\\+_" " Rope")
           sml/shorten-modes nil
           sml/mode-width 16
           sml/name-width 30)

     ))

(sml/setup)

(provide 'modeline-settings)
;; modeline-settings ends here.
;;;
