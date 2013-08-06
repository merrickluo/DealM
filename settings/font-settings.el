;; -*- Emacs-Lisp -*-
;; Last modified: <2013-06-11 23:14:10 Tuesday by wongrichard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.4
;; PUBLIC LICENSE: GPLv3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;line have 80 length;;
(cond ((string= system-type "windows-nt")
       (set-face-attribute 'default nil :font "Consolas-13"))
      ((string= system-type "darwin")
       (set-face-attribute 'default nil :font "monaco-16"))
      ((string= system-type "gnu/linux")
      (set-face-attribute 'default nil :font "Monofur-16")))

;; Set chinese font
(cond ((string= system-type "windows-nt")
       (set-fontset-font "fontset-default"
                         'gb18030 '("Microsoft Yahei" . "unicode-bmp")))
      ((string= system-type "gnu/linux")
       (set-fontset-font "fontset-default"
                         'gb18030 '("Zhunyuan" . "unicode-bmp"))))

(require 'unicode-fonts)

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
