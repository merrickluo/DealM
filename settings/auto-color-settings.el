;; -*- Emacs-Lisp -*-
;; Last modified: <2012-07-26 09:03:08 Thursday by richard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.2
;; PUBLIC LICENSE: GPLv3
;; all speacial color.


;; rainbow-mode
;; ------------------------------------------------------------------
(add-to-list 'load-path (concat plugins-path-r "rainbow-mode"))

(autoload 'rainbow-mode "rainbow-mode" "\
Colorize strings that represent colors.
This will fontify with colors the string like \"#aabbcc\" or \"blue\".

\(fn &optional ARG)" t nil)

(dolist (hook '(html-mode-hook
                emacs-lisp-mode-hook))
  (add-hook hook 'rainbow-mode))

;; color-file settings.
;; ------------------------------------------------------------------
(add-to-list 'load-path (concat plugins-path-r "color-file-completion-1.0.1"))
(require 'color-file-completion)

(provide 'auto-color-settings)
;; auto-color-settings ends here.
;;;
