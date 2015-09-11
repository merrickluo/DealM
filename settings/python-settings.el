;; -*- Emacs-Lisp -*-
;; Last modified: <2015-09-11 19:06:18 Friday by wongrichard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1
;; PUBLIC LICENSE: GPLv3

(autoload 'python-mode "python" "python Editing Mode" t)

(setq jedi:complete-on-dot t
      jedi:tooltip-method nil
      jedi:key-goto-definition (kbd "C-c g")
      jedi:environment-root "jedi"
      jedi:environment-virtualenv '("--python" "/usr/local/bin/python3")
      jedi:server-command (list "python3" (concat plugins-path-r "emacs-jedi/jediepcserver.py")))

(add-to-list 'load-path (concat plugins-path-r "emacs-jedi/"))
(add-to-list 'load-path (concat plugins-path-r "emacs-python-environment/"))
(require 'python-environment)

(autoload 'jedi:ac-setup "jedi" "" t)
(autoload 'jedi-mode "jedi" "" t)

(setq py-smart-indentation t
      py-shell-name "python3"
      python-shell-interpreter py-shell-name
      python-default-indent-offset 4
      python-indent-offset 4
      py-indent-offset python-indent-offset)

(autoload 'nosetests-all        "nose" "" t)
(autoload 'nosetests-module     "nose" "" t)
(autoload 'nosetests-one        "nose" "" t)
(autoload 'nosetests-pdb-all    "nose" "" t)
(autoload 'nosetests-pdb-module "nose" "" t)
(autoload 'nosetests-pdb-one    "nose" "" t)

(eval-after-load "quickrun"
  '(progn
     (message "quickrun eval triggered")
     (quickrun-add-command "python"
                           '((:command . "ss && python3")
                             (:description . "Run Python 3 script"))
                           :default "python"
                           :mode 'python-mode
                           :override t)
     )
  )


(defun python-dev-settings()

  ;; set environment conform my python3 venv rules
  ;; alias ss='source ../.env/"${PWD##*/}"/bin/activate'
  ;; alias pss='python3 -m venv ../.env/"${PWD##*/}"'
  ;; alias pss2='virtualenv ../.env/"${PWD##*/}"'
  ;; alias rmss='rm -rf ../.env/"${PWD##*/}"'
  (when (projectile-project-p)
    (set (make-local-variable 'jedi:server-command)
         (list "python3" (concat plugins-path-r "emacs-jedi/jediepcserver.py")
               "--virtual-env" (concat (projectile-project-root) "../.env/" (projectile-project-name))))
    )
  (jedi:ac-setup)
  (jedi-mode 1)
  (local-set-key "\C-x\M-j" 'run-python)
  (local-set-key "\C-x\M-w" 'py-copy-clause)
  (local-set-key "\C-x\C-p" 'py-up-clause)
  (local-set-key "\C-x\C-n" 'py-down-clause)
  (local-set-key "\C-cta"   'nosetests-all)
  (local-set-key "\C-ctm"   'nosetests-module) ;; C-c m conflicts w/ pylint
  (local-set-key "\C-ct."   'nosetests-one)
  (local-set-key "\C-ctt"   'nosetests-one)
  (local-set-key "\C-cpa"   'nosetests-pdb-all)
  (local-set-key "\C-cpm"   'nosetests-pdb-module)
  (local-set-key "\C-cp."   'nosetests-pdb-one))

(add-hook 'python-mode-hook 'python-dev-settings)


(provide 'python-settings)
;; python-settings ends here.
;;;
