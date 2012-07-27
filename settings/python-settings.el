;; -*- Emacs-Lisp -*-
;; Last modified: <2012-07-27 21:12:11 Friday by richard>

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

;; pymacs settings.

(add-to-list 'load-path (concat emacs-root-path "python-libs/Pymacs/"))

(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

(defun setup-ropemacs ()
  "Setup the ropemacs harness"
  ;; python environment
  ;; ------------------------------------------------------------------
  (setenv "PYMACS_PYTHON" "python2")

  (message "****************************")
  (if (and (getenv "PYTHONPATH") (not (string= (getenv "PYTHONPATH") "")))
      (setenv "PYTHONPATH"
              (concat
               (getenv "PYTHONPATH") path-separator
               (concat emacs-root-path "python-libs/" path-separator
                       emacs-root-path "python-libs/Pymacs/")))
    (setenv "PYTHONPATH"
            (concat emacs-root-path "python-libs/Pymacs/" path-separator
                    emacs-root-path "python-libs/")))
  (message (concat "Current PYTHONPATH is " (getenv "PYTHONPATH")))
  (message "****************************")

  (setq pymacs-python-command "python2")

  (pymacs-load "ropemacs" "rope-")

  ;; Stops from erroring if there's a syntax err
  (setq ropemacs-codeassist-maxfixes 3)

  ;; Configurations
  (setq ropemacs-guess-project t)
  (setq ropemacs-enable-autoimport t)

  (setq ropemacs-autoimport-modules '("os" "shutil" "sys" "logging"
				      "django.*"))



  ;; Adding hook to automatically open a rope project if there is one
  ;; in the current or in the upper level directory
  (add-hook 'python-mode-hook
            (lambda ()
              (cond ((file-exists-p ".ropeproject")
                     (rope-open-project default-directory))
                    ((file-exists-p "../.ropeproject")
                     (rope-open-project (concat default-directory "..")))
                    )))
  )

;; Ipython integration with fgallina/python.el
(defun epy-setup-ipython ()
  "Setup ipython integration with python-mode"
  (interactive)
  (setq
   python-shell-interpreter "ipython2"
   python-shell-interpreter-args ""
   python-shell-prompt-regexp "In \[[0-9]+\]: "
   python-shell-prompt-output-regexp "Out\[[0-9]+\]: "
   python-shell-completion-setup-code ""
   python-shell-completion-string-code "';'.join(__IP.complete('''%s'''))\n")
  )


;; python mode list
;; ------------------------------------------------------------------
(eval-after-load 'python
  '(progn
     ;;==================================================
     ;; Ropemacs Configuration
     ;;==================================================
     (setup-ropemacs)

     ;;==================================================
     ;; Virtualenv Commands
     ;;==================================================
     (autoload 'virtualenv-activate "virtualenv"
       "Activate a Virtual Environment specified by PATH" t)
     (autoload 'virtualenv-workon "virtualenv"
       "Activate a Virtual Environment present using virtualenvwrapper" t)

     ;; when we swich on the command line, switch in Emacs
     (desktop-save-mode 1)
     (defun workon-postactivate (virtualenv)
       (require 'virtualenv)
       (virtualenv-activate virtualenv)
       (desktop-change-dir virtualenv))
     )
  )

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
