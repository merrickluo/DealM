;; -*- Emacs-Lisp -*-
;; Last modified: <2012-11-11 10:18:15 Sunday by June>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.4
;; PUBLIC LICENSE: GPLv3

(if (string= system-type "windows-nt")
    (set-face-attribute 'default nil :font "Consolas-13")
  (set-face-attribute 'default nil :font "Inconsolata-14"))

;; Set chinese font
(if (string= system-type "windows-nt")
    (set-fontset-font "fontset-default"
                      'gb18030 '("Microsoft Yahei" . "unicode-bmp"))
  (set-fontset-font "fontset-default"
                    'gb18030 '("Zhunyuan" . "unicode-bmp")))

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
