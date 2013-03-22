;; -*- Emacs-Lisp -*-
;; Last modified: <2013-03-22 15:54:40 Friday by richard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1
;; PUBLIC LICENSE: GPLv3

(autoload 'python-mode "python" "python Editing Mode" t)

(setq interpreter-mode-alist (cons '("python2" . python-mode)
                                   interpreter-mode-alist))

(setq py-python-command-args '( "--colors" "Linux")
      py-smart-indentation t
      py-shell-name "python2"
      py-indent-offset 4)

(defconst pythonlib-path-r
  (concat emacs-root-path "python-lib/") "path of python libs used by emacs.")


(add-to-list 'load-path (concat plugins-path-r "emacs-jedi"))
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)



;; python mode list
;; ------------------------------------------------------------------
(eval-after-load 'python
  '(progn

     ;;==================================================
     ;; Virtualenv Commands
     ;;==================================================
     (autoload 'virtualenv-activate "virtualenv"
       "Activate a Virtual Environment specified by PATH" t)
     (autoload 'virtualenv-workon "virtualenv"
       "Activate a Virtual Environment present using virtualenvwrapper" t)
     (setq python-indent-offset 4
           python-indent-guess-indent-offset nil
           python-shell-interpreter "python2"
           ;; python-shell-interpreter "ipython2" ;; make ipython2 default.
           python-shell-interpreter-args ""
           python-shell-prompt-regexp "In \[[0-9]+\]: "
           python-shell-prompt-output-regexp "Out\[[0-9]+\]: "
           python-shell-completion-setup-code ""
           python-shell-completion-string-code "';'.join(__IP.complete('''%s'''))\n")

     (defun workon-postactivate (virtualenv)
       (require 'virtualenv)
       (virtualenv-activate virtualenv)
       (desktop-change-dir virtualenv))
     ;; (desktop-save-mode 1)
     ))



(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxi\\'" . cython-mode))

;; Py3 files
(add-to-list 'auto-mode-alist '("\\.py3\\'" . python-mode))


;; key
;; ------------------------------------------------------------------
(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map "\C-m" 'newline-and-indent)))

(provide 'python-settings)
;; python-settings ends here.
;;;
