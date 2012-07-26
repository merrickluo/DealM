;; -*- Emacs-Lisp -*-
;; Last modified: <2012-07-26 09:27:39 Thursday by richard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.2
;; PUBLIC LICENSE: GPLv3

(add-to-list 'load-path (concat plugins-path-r "magit"))


;; magit settings.
(autoload 'magit-status "magit" "\
Open a Magit status buffer for the Git repository containing
DIR.  If DIR is not within a Git repository, offer to create a
Git repository in DIR.

Interactively, a prefix argument means to ask the user which Git
repository to use even if `default-directory' is under Git control.
Two prefix arguments means to ignore `magit-repo-dirs' when asking for
user input.

\(fn DIR)" t nil)

;;;***

(autoload 'magit-stgit-mode "magit-stgit" "\
StGit support for Magit

\(fn &optional ARG)" t nil)

(autoload 'turn-on-magit-stgit "magit-stgit" "\
Unconditionally turn on `magit-stgit-mode'.

\(fn)" nil nil)

;;;***
;;;### (autoloads (turn-on-magit-svn magit-svn-mode) "magit-svn"
;;;;;;  "magit-svn.el" (20476 13736))
;;; Generated autoloads from magit-svn.el

(autoload 'magit-svn-mode "magit-svn" "\
SVN support for Magit

\(fn &optional ARG)" t nil)

(autoload 'turn-on-magit-svn "magit-svn" "\
Unconditionally turn on `magit-svn-mode'.

\(fn)" nil nil)

;;;***
;;;### (autoloads (turn-on-magit-topgit magit-topgit-mode) "magit-topgit"
;;;;;;  "magit-topgit.el" (20476 13736))
;;; Generated autoloads from magit-topgit.el

(autoload 'magit-topgit-mode "magit-topgit" "\
Topgit support for Magit

\(fn &optional ARG)" t nil)

(autoload 'turn-on-magit-topgit "magit-topgit" "\
Unconditionally turn on `magit-topgit-mode'.

\(fn)" nil nil)


;; FIXME: now obsolete..
(defun select-vc-status ()
  "Calls for a directory and calls `svn-status' or `git-status' depending on what
type of version control found in that directory"
  (interactive)
  (let* ((local-default-dir (ftf-project-directory))
         (targetDir
          (read-directory-name "Status of directory: "
                               local-default-dir
                               local-default-dir
                               nil)))
    (cond ((file-exists-p (concat targetDir "/.git"))
           (magit-status targetDir))
          ((file-exists-p (concat targetDir "/.svn"))
           (svn-status targetDir))
          ((file-exists-p (concat targetDir "/CVS"))
           (cvs-status targetDir)))))


;; change magit diff colors
(eval-after-load 'magit
  '(progn
     (set-face-attribute 'magit-diff-add nil         :foreground "green1")
     (set-face-attribute 'magit-diff-del nil         :foreground "red1")
     (set-face-attribute 'magit-diff-file-header nil :foreground "RoyalBlue1")
     (set-face-attribute 'magit-diff-hunk-header nil :foreground "#fbde2d")))


;; keys in magit...
(global-set-key  (kbd "C-x v z") 'magit-status)

(add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map [?c] 'magit-status)))

(provide 'git-settings)
;; git-settings ends here.
;;;
