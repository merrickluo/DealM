;; -*- Emacs-Lisp -*-
;; Last modified: <2015-11-30 13:21:19 Monday by wongrichard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.3
;; PUBLIC LICENSE: GPLv3

(add-to-list 'load-path (concat plugins-path-r "magit/lisp"))
(add-to-list 'load-path (concat plugins-path-r "git-modes"))



;; magit settings.
(autoload 'magit-status "magit" "\
Show the status of the current Git repository in a buffer.
With a prefix argument prompt for a repository to be shown.
With two prefix arguments prompt for an arbitrary directory.
If that directory isn't the root of an existing repository
then offer to initialize it as a new repository.

\(fn &optional DIRECTORY)" t nil)




;; change magit diff colors
;; (eval-after-load 'magit
;;   '(progn
;;      (set-face-attribute 'magit-item-highlight   nil :background "grey20"
;;                          :inherit    nil)
;;      (set-face-attribute 'magit-diff-add         nil :foreground "green1")
;;      (set-face-attribute 'magit-diff-del         nil :foreground "red1")
;;      (set-face-attribute 'magit-diff-file-header nil :foreground "RoyalBlue1")
;;      (set-face-attribute 'magit-diff-hunk-header nil :foreground "#fbde2d")))

(add-hook 'magit-key-mode-popup-committing
          (lambda () (progn (make-local-variable scroll-margin)
                            (setq scroll-margin 0))))

;; keys in magit...
(global-set-key  (kbd "C-x v z") 'magit-status)
(setq magit-push-always-verify nil)

(add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map [?c] 'magit-status)))

(provide 'git-settings)
;; git-settings ends here.
;;;
