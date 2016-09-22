;; -*- Emacs-Lisp -*-
;; Last modified: <2016-09-22 23:44:54 Thursday by richard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.5
;; PUBLIC LICENSE: GPLv3

(add-to-list 'load-path (concat plugins-path-r "emacs-quickrun/"))

(use-package quickrun
  :defer t
  :commands (quickrun quickrun-shell)
  :bind (("C-c r" . quickrun)))

(use-package compile
  :defer t
  :init
  (setq compilation-scroll-output t
        compile-command "make -k")
  :config
  (defun after-debug-kill-buffer (process state)
    (message "%s" state)
    (if (string-match "finished" state)
        (kill-buffer (current-buffer))))

  (add-hook 'gdb-mode-hook
            (lambda ()
              (set-process-sentinel (get-buffer-process (current-buffer))
                                    #'after-debug-kill-buffer)))
  :bind (:map compilation-mode-map
              ("n" . compilation-next-error)
              ("p" . compilation-previous-error)
              ("u" . View-scroll-half-page-backward)
              ("f" . am-forward-word-or-to-word)
              ("d" . scroll-up)
              ("w" . scroll-down)))

(use-package smart-compile
  :defer t
  :bind (("C-c C-m" . smart-compile))
  :config
  (add-to-list 'smart-compile-alist
               '(python-mode    .  "source ../.env/\"${PWD##*/}\"/bin/activate && python3 %f"))
  (add-to-list 'smart-compile-alist
               '(jade-mode      .  "pyjade -c jinja %f"))
  (add-to-list 'smart-compile-alist
               '(objc-mode      .  "clang -fobjc-arc %f -o %n")))

(provide 'compile-settings)
;; compile-settings ends here.
;;;
