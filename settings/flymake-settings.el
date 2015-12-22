;; -*- Emacs-Lisp -*-
;; Last modified: <2015-12-22 14:07:09 Tuesday by wongrichard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.2
;; PUBLIC LICENSE: GPLv3

(autoload 'flymake-find-file-hook "flymake" "" t)

(add-hook 'find-file-hook 'flymake-find-file-hook)

(defun flymake-settings()
  "settings for flymake"
  ;; display the buffer-relative positions of flymake errors and warnings on the fringe.

  (require 'flymake-cursor)

  (setq temporary-file-directory "/tmp/")

  (defvar flymake-mode-map (make-sparse-keymap))

  (setq flymake-gui-warnings-enabled nil)
  ;; language grammar check
  (setq-default ispell-program-name "aspell")
  (custom-set-faces
   '(flymake-errline ((((class color)) (:underline "red"))))
   '(flymake-warnline ((((class color)) (:underline "yellow")))))

  (defvar flymake-makefile-filenames '("Makefile" "makefile" "GNUmakefile") "File names for make.")

  (defun flymake-get-gcc-cmdline (source base-dir)
    (let (found)
      (dolist (makefile flymake-makefile-filenames)
        (and (file-readable-p (concat base-dir "/" makefile))
             (setq found t)))
      (if found
          (list "make"
                (list "-s"
                      "-C"
                      base-dir
                      (concat "CHK_SOURCES=" source)
                      "SYNTAX_CHECK_MODE=1"
                      "check-syntax"))
        (let ((cc (if (string= (file-name-extension source) "c") "gcc" "g++")))
          (list cc
                (list "-Wall"
                      "-Wextra"
                      "-pedantic"
                      "-fsyntax-only"
                      "-I.."
                      "-I../include"
                      "-I../inc"
                      "-I../common"
                      "-I../public"
                      "-I../.."
                      "-I../../include"
                      "-I../../inc"
                      "-I../../common"
                      "-I../../public"
                      source))))))

  (defun flymake-init-find-makfile-dir (source-file-name)
    "Find Makefile, store its dir in buffer data and return its dir, if found."
    (let* ((source-dir (file-name-directory source-file-name))
           (buildfile-dir nil))
      (catch 'found
        (dolist (makefile flymake-makefile-filenames)
          (let ((found-dir (flymake-find-buildfile makefile source-dir)))
            (when found-dir
              (setq buildfile-dir found-dir)
              (setq flymake-base-dir buildfile-dir)
              (throw 'found t)))))
      buildfile-dir))
  (defun flymake-simple-make-gcc-init-impl (create-temp-f
                                            use-relative-base-dir
                                            use-relative-source)
    "Create syntax check command line for a directly checked source file.
Use CREATE-TEMP-F for creating temp copy."
    (let* ((args nil)
           (source-file-name buffer-file-name)
           (source-dir (file-name-directory source-file-name))
           (buildfile-dir
            (and (executable-find "make")
                 (flymake-init-find-makfile-dir source-file-name)))
           (cc (if (string= (file-name-extension source-file-name) "c")
                   "gcc"
                 "g++")))
      (if (or buildfile-dir (executable-find cc))
          (let* ((temp-source-file-name
                  (ignore-errors
                    (flymake-init-create-temp-buffer-copy create-temp-f))))
            (if temp-source-file-name
                (setq args
                      (flymake-get-syntax-check-program-args
                       temp-source-file-name
                       (if buildfile-dir buildfile-dir source-dir)
                       use-relative-base-dir
                       use-relative-source
                       (if buildfile-dir
                           'flymake-get-make-cmdline
                         'flymake-get-gcc-cmdline)))
              (flymake-report-fatal-status
               "TMPERR"
               (format "Can't create temp file for %s" source-file-name))))
        (flymake-report-fatal-status
         "NOMK" (format "No buildfile (%s) found for %s, or can't found %s"
                        "Makefile" source-file-name cc)))
      args))


  (defun flymake-simple-make-gcc-init ()
    (flymake-simple-make-gcc-init-impl 'flymake-create-temp-inplace t t))

  ;; Use the  option "-file-line-error", you can use:
  (defun flymake-get-tex-args (file-name) (list "chktex" (list "-q" "-v0" file-name)))

  (defun flymake-master-make-gcc-init (get-incl-dirs-f
                                       master-file-masks
                                       include-regexp)
    "Create make command line for a source file
 checked via master file compilation."
    (let* ((args nil)
           (temp-master-file-name
            (ignore-errors
              (flymake-init-create-temp-source-and-master-buffer-copy
               get-incl-dirs-f
               'flymake-create-temp-inplace
               master-file-masks
               include-regexp)))
           (cc (if (string= (file-name-extension buffer-file-name) "c")
                   "gcc"
                 "g++")))
      (if temp-master-file-name
          (let* ((source-file-name buffer-file-name)
                 (source-dir (file-name-directory source-file-name))
                 (buildfile-dir
                  (and (executable-find "make")
                       (flymake-init-find-makfile-dir source-file-name))))
            (if (or buildfile-dir (executable-find cc))
                (setq args (flymake-get-syntax-check-program-args
                            temp-master-file-name
                            (if buildfile-dir buildfile-dir source-dir)
                            nil
                            nil
                            (if buildfile-dir
                                'flymake-get-make-cmdline
                              'flymake-get-gcc-cmdline)))
              (flymake-report-fatal-status
               "NOMK"
               (format "No buildfile (%s) found for %s, or can't found %s"
                       "Makefile" source-file-name cc))))
        (flymake-report-fatal-status
         "TMPERR" (format "Can't create temp file for %s" source-file-name)))
      args))

  (defun flymake-create-temp-in-system-tempdir (filename prefix)
    (make-temp-file (or prefix "flymake")))
  ;; Learn code from https://github.com/akaihola/flymake-python
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-in-system-tempdir))

           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list (concat emacs-root-path "python-lib/pyflymake.py")
            (list local-file))))
      ;;     check path

  (defun flymake-html-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "tidy" (list local-file))))
  (add-to-list 'flymake-err-line-patterns
	             '("line \\([0-9]+\\) column \\([0-9]+\\) - \\(Warning\\|Error\\): \\(.*\\)"
	               nil 1 2 4))

  (setq flymake-allowed-file-name-masks
        '(("\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'" flymake-simple-make-gcc-init)
          ("\\.cs\\'" flymake-simple-make-init)
          ("\\.\\(?:h\\(?:pp\\)?\\)\\'" flymake-master-make-header-init flymake-master-cleanup)
          ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup)
          ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup)
          ("\\.tex\\'" flymake-simple-tex-init flymake-simple-cleanup)
          ("\\.idl\\'" flymake-simple-make-init)
          ("\\.py\\'" flymake-pylint-init)))

  (defalias 'goto-next-error 'flymake-goto-next-error "Go to next error position")
  (defalias 'goto-prev-error 'flymake-goto-prev-error "Go to previous error position")

  (define-key flymake-mode-map (kbd "C-c p") 'flymake-goto-prev-error)
  (define-key flymake-mode-map (kbd "C-c n") 'flymake-goto-next-error)
  (or (assoc 'flymake-mode minor-mode-map-alist)
      (setq minor-mode-map-alist
            (cons (cons 'flymake-mode flymake-mode-map)
                  minor-mode-map-alist)))
  )

(eval-after-load "flymake"
  '(flymake-settings))
(provide 'flymake-settings)
;; flymake-settings ends here.
;;;
