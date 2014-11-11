;; -*- Emacs-Lisp -*-
;; Last modified: <2014-11-11 14:25:55 Tuesday by wongrichard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1
;; PUBLIC LICENSE: GPLv3

(autoload 'python-mode "python" "python Editing Mode" t)


(setq jedi:complete-on-dot t
      jedi:key-goto-definition (kbd "C-c g"))

(add-to-list 'load-path (concat plugins-path-r "emacs-jedi/"))
(add-to-list 'load-path (concat plugins-path-r "emacs-python-environment/"))
(require 'python-environment)

(autoload 'jedi:ac-setup "jedi" "" t)
(autoload 'jedi-mode "jedi" "" t)

(setq py-python-command-args '( "--colors" "Linux")
      py-smart-indentation t
      py-shell-name "python2"
      py-indent-offset 4)

(autoload 'nosetests-all        "nose" "" t)
(autoload 'nosetests-module     "nose" "" t)
(autoload 'nosetests-one        "nose" "" t)
(autoload 'nosetests-pdb-all    "nose" "" t)
(autoload 'nosetests-pdb-module "nose" "" t)
(autoload 'nosetests-pdb-one    "nose" "" t)

(defun python-dev-settings()
    (jedi:ac-setup)
    (jedi-mode 1)
    (local-set-key "\C-x\M-w"  'py-copy-clause)
    (local-set-key "\C-x\C-p"  'py-up-clause)
    (local-set-key "\C-x\C-n"  'py-down-clause)
    (local-set-key "\C-cta"  'nosetests-all)
    (local-set-key "\C-ctm"  'nosetests-module) ;; C-c m conflicts w/ pylint
    (local-set-key "\C-ct."  'nosetests-one)
    (local-set-key "\C-ctt"  'nosetests-one)
    (local-set-key "\C-cpa" 'nosetests-pdb-all)
    (local-set-key "\C-cpm" 'nosetests-pdb-module)
    (local-set-key "\C-cp." 'nosetests-pdb-one))
(add-hook 'python-mode-hook 'python-dev-settings)
(when (string= system-type "darwin")
;;; ENV path correction for (Mac os x)
  (setenv "PYTHONPATH"
          (concat
           (getenv "PYTHONPATH") path-separator
           "/Library/Python/2.7/site-packages/" path-separator
           "/usr/local/lib/python2.7/site-packages/")))

(provide 'python-settings)
;; python-settings ends here.
;;;
