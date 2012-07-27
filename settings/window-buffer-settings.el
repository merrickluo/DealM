;; -*- Emacs-Lisp -*-
;; Last modified: <2012-07-27 08:22:07 Friday by richard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1
;; PUBLIC LICENSE: GPLv3


;; fullscreen settings
;; ------------------------------------------------------------------
(defun fullscreen ()
  "Fullscreen."
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         ;; if first parameter is '1', can't toggle fullscreen status
                         '(1 "_NET_WM_STATE_FULLSCREEN" 0)))

;;; autoload
(defun fullscreen-toggle ()
  "Toggle fullscreen status."
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         ;; if first parameter is '2', can toggle fullscreen status
                         '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

(fullscreen)



(provide 'window-buffer-settings)
;; window-buffer-settings ends here.
;;;
