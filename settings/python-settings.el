;; -*- Emacs-Lisp -*-
;; Last modified: <2016-08-27 08:22:52 Saturday by wongrichard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1
;; PUBLIC LICENSE: GPLv3

(use-package
 python
 :init                                 ; before load package
 (setq py-smart-indentation t
       py-shell-name "python3"
       python-shell-interpreter py-shell-name
       python-default-indent-offset 4
       python-indent-offset 4
       py-indent-offset python-indent-offset)
 :bind (:map python-mode-map          ; bind keys in specific map
             ("C-x M-j" . run-python)
             ("C-x M-w" . py-copy-clause)
             ("C-x C-p" . py-up-clause)
             ("C-x C-n" . py-down-clause))
 :config                               ; after load package
 (progn
   ;; set environment conform my python3 venv rules
   ;; alias ss='source ../.env/"${PWD##*/}"/bin/activate'
   ;; alias pss='python3 -m venv ../.env/"${PWD##*/}"'
   ;; alias pss2='virtualenv ../.env/"${PWD##*/}"'
   ;; alias rmss='rm -rf ../.env/"${PWD##*/}"'
   (when (projectile-project-p)
     (set (make-local-variable 'jedi:server-command)
          (list "python3" (concat plugins-path-r "emacs-jedi/jediepcserver.py")
                "--virtual-env" (concat (projectile-project-root) "../.env/" (projectile-project-name)))

          ))
   ;; TODO: need cancel the lines after deactivate the python.
   ;; (let ((virtualenv-path
   ;;        (concat (projectile-project-root) "../.env/" (projectile-project-name))))
   ;;   (unless (member virtualenv-path exec-path)
   ;;     (add-to-list 'exec-path virtualenv-path)
   ;;     (setenv "PATH" (mapconcat 'identity exec-path ":"))))

   (jedi:ac-setup)
   (jedi-mode 1))

 :mode ("\\.py\\'" . python-mode)      ; mode name is diff from package
 :interpreter ("python" . python-mode)
 :mode "\\.rb\\'"
 :interpreter "ruby")

(add-to-list 'load-path (concat plugins-path-r "emacs-jedi/"))
(add-to-list 'load-path (concat plugins-path-r "emacs-python-environment/"))

(use-package
 jedi
 :init                                 ; before load package
 (setq jedi:complete-on-dot t
       jedi:tooltip-method nil
       jedi:key-goto-definition (kbd "C-c g")
       jedi:environment-root "jedi"
       jedi:environment-virtualenv '("--python" "/usr/local/bin/python3")
       jedi:server-command (list "python3" (concat plugins-path-r "emacs-jedi/jediepcserver.py"))))

(use-package
 nose
 ;; the :commands :bind keyword create autoloads for those commands and defers loading of the module until they are used.
 :bind (:map python-mode
        ("C-c t a" . nosetests-all)
        ("C-c t m" . nosetests-module) ;; C-c m conflicts w/ pylint
        ("C-c t ." . nosetests-one)
        ("C-c t t" . nosetests-one)
        ("C-c p a" . nosetests-pdb-all)
        ("C-c p m" . nosetests-pdb-module)
        ("C-c p ." . nosetests-pdb-one)))

(use-package
 quickrun
 :config                               ; after load package
 (quickrun-add-command "python"
                       '((:command . "ss && python3")
                         (:description . "Run Python 3 script"))
                       :default "python"
                       :mode 'python-mode
                       :override t))

(provide 'python-settings)
;; python-settings ends here.
;;;
