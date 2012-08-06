;; -*- Emacs-Lisp -*-
;; Last modified: <2012-07-31 11:24:47 Tuesday by richard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.4
;; PUBLIC LICENSE: GPLv3
(require 'compile+)

(autoload 'smart-compile "smart-compile" "" t)


;; compile-misc need smart compile inside.
(autoload 'compile-buffer "compile-misc" "" t)
(autoload 'run-program    "compile-misc" "" t)
(autoload 'make           "compile-misc" "" t)
(autoload 'make-check     "compile-misc" "" t)
(autoload 'make-clean     "compile-misc" "" t)
(autoload 'ant            "compile-misc" "" t)
(autoload 'ant-clean      "compile-misc" "" t)
(autoload 'ant-test       "compile-misc" "" t)
(autoload 'make-install   "compile-misc" "" t)


(eval-after-load "smart-compile"
                '(add-to-list 'smart-compile-alist '(python-mode    .  "python2 %f")))

(defun compile-settings ()
  "Settings for `compile'."
  ;; 设置编译命令
  (setq compile-command "make -k")

  (defun after-debug-kill-buffer (process state)
    (message "%s" state)
    (if (string-match "finished" state)
        (kill-buffer (current-buffer))))

  (add-hook 'gdb-mode-hook
            (lambda ()
              (set-process-sentinel (get-buffer-process (current-buffer))
                                    #'after-debug-kill-buffer)))

  ;; shortcuts
  ;; ------------------------------------------------------------------
  (global-set-key (kbd "M-n") 'next-error)
  (global-set-key (kbd "M-p") 'previous-error)

  (defun compile-keys ()
    (local-set-key (kbd "C-c C-m")  'make)
    (local-set-key (kbd "C-c m")    'make-check)
    (local-set-key (kbd "C-c M")    'make-clean)
    (local-set-key (kbd "C-c c")    'compile-buffer)
    (local-set-key (kbd "C-c r")    'run-program))
  (dolist (hook '(c-mode-base-hook
                  sh-mode-hook
                  compilation-mode-hook
                  ruby-mode-hook
                  python-mode-hook))
    (add-hook hook 'compile-keys))


  (setq compilation-scroll-output t)

  (define-key compilation-mode-map "n" 'compilation-next-error)
  (define-key compilation-mode-map "p" 'compilation-previous-error)
  (define-key compilation-mode-map "'" 'switch-to-other-buffer)
  (define-key compilation-mode-map "u" 'View-scroll-half-page-backward)
  (define-key compilation-mode-map "f" 'am-forward-word-or-to-word)
  (define-key compilation-mode-map "d" 'scroll-up)
  (define-key compilation-mode-map "w" 'scroll-down))


(eval-after-load "compile"
  '(compile-settings))
(provide 'compile-settings)
;; compile-settings ends here.
;;;