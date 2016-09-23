;; -*- Emacs-Lisp -*-
;; Last modified: <2016-09-23 11:43:57 Friday by richard>

;; Copyright (C) 2012-2013 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.2
;; PUBLIC LICENSE: GPLv3

(use-package dired-x
  :defer t
  :commands
  (dired-omit-mode)
  :init     ; before code load
  (add-hook 'dired-mode-hook
            #'(lambda ()
                (dired-omit-mode t))))

(use-package dired
  :defer t
  :init
  (setq dired-dwim-target t)
  :bind (:map dired-mode-map
         ("C-o" . dired-display-file)
         ("M-b" . backward-word)))

(use-package dired+
  :after (dired)
  :defer t
  :config   ; execute code after a package is loaded
  (autoload 'openwith-mode "openwith" t)
  (openwith-mode t)

  (if (string= system-type "darwin")
      (progn
        (push '("\\.pdf\\'" "open" (file)) openwith-associations))
    (progn
      (push '("\\.pdf\\'" "evince" (file)) openwith-associations)
      (push '("\\.epub\\'" "calibre" (file)) openwith-associations)
      (push '("\\.rar\\'" "peazip" (file)) openwith-associations)))
  (define-key dired-mode-map "1" 'delete-other-windows)
  (define-key dired-mode-map "2" 'split-window-vertically)
  (define-key dired-mode-map "3" 'split-window-horizontally)
  (define-key dired-mode-map "o" 'other-window)
  ;; From color-moccur
  (define-key dired-mode-map "O" 'dired-do-moccur)
  (defvar dired-sort-map (make-sparse-keymap))
  (define-key dired-mode-map "s" dired-sort-map)
  (define-key dired-sort-map "s" (lambda () "sort by Size" (interactive) (dired-sort-other (concat dired-listing-switches " -S"))))
  (define-key dired-sort-map "x" (lambda () "sort by eXtension" (interactive) (dired-sort-other (concat dired-listing-switches " -X"))))
  (define-key dired-sort-map "t" (lambda () "sort by Time" (interactive) (dired-sort-other (concat dired-listing-switches " -t"))))
  (define-key dired-sort-map "n" (lambda () "sort by Name" (interactive) (dired-sort-other dired-listing-switches)))
  (define-key dired-sort-map "?" (lambda () "sort help" (interactive) (message "s Size; x eXtension; t Time; n Name")))
  (define-key dired-sort-map "h" (lambda () "sort help" (interactive) (message "s Size; x eXtension; t Time; n Name")))
  (define-key dired-sort-map "/" (lambda () "sort help" (interactive) (message "s Size; x eXtension; t Time; n Name"))))



(provide 'dired-settings)
;; dired-settings ends here.
;;;
