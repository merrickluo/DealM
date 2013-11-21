;; -*- Emacs-Lisp -*-
;; Last modified: <2013-11-02 12:31:20 Saturday by wongrichard>

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
  (concat emacs-root-path "python-lib/") "path of python libs used by emacs.")
(defconst pymacs-path-r
  (concat pythonlib-path-r) "path of pymacs.")
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
  (let ((pythonlib-path
         (mapconcat 'identity
                    (list pythonlib-path-r pymacs-path-r)
                    path-separator)))
    (if (and (getenv "PYTHONPATH")
             (not (string= (getenv "PYTHONPATH") "")))
        (unless (string-prefix-p pythonlib-path (getenv "PYTHONPATH"))
          (setenv "PYTHONPATH"
                  (concat pythonlib-path (getenv "PYTHONPATH"))))
      (setenv "PYTHONPATH" pythonlib-path)))
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


;; key
;; ------------------------------------------------------------------
(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map "\C-m" 'newline-and-indent)))

(when (string= system-type "darwin")
;;; ENV path correction for (Mac os x)
  (setenv "PYTHONPATH"
          (concat
           (getenv "PYTHONPATH") path-separator
           "/Library/Python/2.7/site-packages/" path-separator
           "/usr/local/lib/python2.7/site-packages/" path-separator
           )))


(provide 'python-settings)
;; python-settings ends here.
;;;
