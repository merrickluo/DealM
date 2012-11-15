;; -*- Emacs-Lisp -*-
;; Last modified: <2012-08-22 19:06:59 Wednesday by richard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1
;; PUBLIC LICENSE: GPLv3
;; include cedet and cedet's plugins settings.

;; (require 'cedet (concat plugins-path-r "CEDET/common/cedet.el"))
;; (load-file (concat plugins-path-r "CEDET/common/cedet.el"))
; semantic settings
; --------------------------------------[semantic settings]

;; (global-ede-mode 1)
;; (semantic-load-enable-code-helpers)


;; ; minimum
;; (semantic-load-enable-minimum-features)


;; ; median
;; (semantic-load-enable-code-helpers)

;; ;; excessive
;; (semantic-load-enable-excessive-code-helpers)

;; ;; debuger.
;; (semantic-load-enable-semantic-debugging-helpers)
;; ;; ctags
;; (semantic-load-enable-all-exuberent-ctags-support)



;; (add-to-list 'load-path (concat plugins-path-r "ecb/"))
;; (require 'ecb)

(provide 'cedet-settings)
;; cedet-settings ends here.
;;;
