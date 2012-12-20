;; -*- Emacs-Lisp -*-
;; Last modified: <2012-12-03 11:45:47 Monday by richard>

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

(defconst pythonlib-path-r
  (concat emacs-root-path "python-libs/") "path of python libs used by emacs.")
(defconst pymacs-path-r
  (concat pythonlib-path-r "Pymacs/") "path of pymacs.")
(add-to-list 'load-path pymacs-path-r)

(setq pymacs-python-command "python2"
      pymacs-auto-restart t)

(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")

(eval-after-load "pymacs"
  '(add-to-list 'pymacs-load-path pymacs-path-r))

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
               (concat pythonlib-path-r path-separator
                       pymacs-path-r)))

    (setenv "PYTHONPATH"
            (concat pymacs-path-r path-separator
                    pythonlib-path-r)))
  (message (concat "Current PYTHONPATH is " (getenv "PYTHONPATH")))
  (message "****************************")

  ;; Configurations
  (setq ropemacs-guess-project t
        ropemacs-global-prefix nil
        ropemacs-enable-autoimport t)

  ;; Stops from erroring if there's a syntax err
  (setq ropemacs-codeassist-maxfixes 3)

  (setq ropemacs-autoimport-modules '("os" "shutil" "sys" "logging"
                                      "django.*"))

  (pymacs-load "ropemacs" "rope-")

  ;; Some unittest settings
  ;; ---------------------------------[Some unittest settings]

  (autoload 'nosetests-all        "nose" "" t)
  (autoload 'nosetests-module     "nose" "" t)
  (autoload 'nosetests-one        "nose" "" t)
  (autoload 'nosetests-pdb-all    "nose" "" t)
  (autoload 'nosetests-pdb-module "nose" "" t)
  (autoload 'nosetests-pdb-one    "nose" "" t)


  (add-hook 'python-mode-hook
            (lambda ()
              (local-set-key "\C-ca"  'nosetests-all)
              (local-set-key "\C-cM"  'nosetests-module) ;; C-c m conflicts w/ pylint
              (local-set-key "\C-c."  'nosetests-one)
              (local-set-key "\C-cpa" 'nosetests-pdb-all)
              (local-set-key "\C-cpm" 'nosetests-pdb-module)
              (local-set-key "\C-cp." 'nosetests-pdb-one))
            )

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


     ;; when we swich on the command line, switch in Emacs
     (desktop-save-mode 1)
     (defun workon-postactivate (virtualenv)
       (require 'virtualenv)
       (virtualenv-activate virtualenv)
       (desktop-change-dir virtualenv))
     ;;==================================================
     ;; Ropemacs Configuration
     ;;==================================================
     (setup-ropemacs)

     ;; (desktop-save-mode 1)
     ))

(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxi\\'" . cython-mode))

;; Py3 files
(add-to-list 'auto-mode-alist '("\\.py3\\'" . python-mode))


;; unittest (abl-mode)
;; -----------------------------------------------[unittest]
(autoload 'abl-mode "abl" "" t)



;; key
;; ------------------------------------------------------------------
(add-hook 'python-mode-hook
          '(lambda ()
             (abl-mode)
             (define-key python-mode-map "\C-m" 'newline-and-indent)))

(provide 'python-settings)
;; python-settings ends here.
;;;
