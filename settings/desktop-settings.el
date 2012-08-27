;; -*- Emacs-Lisp -*-
;; Last modified: <2012-08-27 17:26:23 Monday by richard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.2
;; PUBLIC LICENSE: GPLv3

(desktop-save-mode t)
;; desktop autosave
(setq desktop-save t

      desktop-buffers-not-to-save (concat "\\("
                                          "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
                                          "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
                                          "\\)$")

      ;; alway load desktop even when locked
      desktop-load-locked-desktop t)

(dolist (mode '(dired-mode
                Info-mode
                emacs-lisp-mode
                info-lookup-mode
                fundamental-mode))
  (add-to-list 'desktop-modes-not-to-save 'mode))

(add-hook 'auto-save-hook (lambda () (desktop-save-in-desktop-dir)))

(provide 'desktop-settings)
;; desktop-settings ends here.
;;;
