;; -*- Emacs-Lisp -*-
;; Last modified: <2012-08-13 08:50:55 Monday by richard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.2
;; PUBLIC LICENSE: GPLv3

(desktop-save-mode t)
;; desktop autosave
(setq desktop-save t

      desktop-globals-to-save '(search-ring
                                regexp-search-ring
                                tags-table-list
                                tags-file-name)

      ;; alway load desktop even when locked
      desktop-load-locked-desktop t)


(desktop-read (concat emacs-root-path ""))
(provide 'desktop-settings)
;; desktop-settings ends here.
;;;
