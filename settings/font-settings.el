;; -*- Emacs-Lisp -*-
;; Last modified: <2012-07-26 08:57:29 Thursday by richard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.3
;; PUBLIC LICENSE: GPLv3

(set-frame-font "Inconsolata-14")

;; 设置中文字体
(set-fontset-font "fontset-default"
                  'gb18030 '("ZhunYuan" . "unicode-bmp"))

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
