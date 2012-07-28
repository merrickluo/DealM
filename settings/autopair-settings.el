;; -*- Emacs-Lisp -*-
;; Last modified: <2012-07-28 11:09:19 Saturday by richard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1c
;; PUBLIC LICENSE: GPLv3

(require 'autopair)

(autopair-global-mode 1)

;; auto pair to c series
(add-hook 'c-mode-common-hook
          #'(lambda ()
              (setq autopair-dont-pair `(:string (?') :string  (?')))))

;; auto pair to C++
;; (add-hook 'c++-mode-hook
;;           #'(lambda ()
;;               (push '(?< . ?>)
;;                     (getf autopair-extra-pairs :code))))

;; auto pair to python..
(add-hook 'python-mode-hook
          #'(lambda ()
              (push '(?< . ?>)
                    (getf autopair-extra-pairs :comment))
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))


(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (push '(?` . ?')
                    (getf autopair-extra-pairs :comment))
              (push '(?` . ?')
                    (getf autopair-extra-pairs :string))))

(add-hook 'latex-mode-hook
          #'(lambda ()
              (push '(?（ . ?）) ; FIXME: How to set here?
                    (getf autopair-extra-pairs :everywhere))
              (set (make-local-variable 'autopair-handle-action-fns)
                    (list #'autopair-default-handle-action
                          #'autopair-latex-mode-paired-delimiter-action))))


(provide 'autopair-settings)
;; autopair-settings ends here.
