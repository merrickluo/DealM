;; -*- Emacs-Lisp -*-
;; Last modified: <2014-01-27 09:41:26 Monday by wongrichard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.4
;; PUBLIC LICENSE: GPLv3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;line have 80 length;;

(autoload 'unicode-fonts-setup "unicode-fonts"
  "Set up Unicode fonts for FONTSET-NAMES.

FONTSET-NAMES must be a list of strings.  Fontset names
which do not currently exist will be ignored.  The
default value is `unicode-fonts-fontset-names'."
  t)

(add-hook 'org-mode-hook 'unicode-fonts-setup)

(set-face-attribute 'default nil :font "Source Code Pro-14")

(defun special-font()
  "Change special buffer to special font.
Should add to hook"
  (set (make-local-variable 'face-remapping-alist)
       '((default variable-pitch :height 0.70)))
  (custom-set-faces
   '(variable-pitch((t (:family "Anonymous Pro"))))))

(dolist (hook '(speedbar-mode-hook
                gdb-locals-mode-hook
                gdb-registers-mode-hook
                gdb-frames-mode-hook
                gdb-assembler-mode-hook
                gdb-memory-mode-hook
                gdb-inferior-io-mode-hook
                gdb-breakpoints-mode-hook
                gdb-threads-mode-hook
                gdb-mode-hook))
  (add-hook hook 'special-font))


(provide 'font-settings)
;; font-settings ends here.
;;;
