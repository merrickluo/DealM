;; -*- Emacs-Lisp -*-
;; Copyright (C) 2012 Richard Wong

;; Last modified: <2016-09-18 10:59:36 Sunday by richard>

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1c
;; PUBLIC LICENSE: GPLv3

(add-to-list 'load-path (concat plugins-path-r "yasnippet/"))

(use-package
  yasnippet
  :commands (yas-global-mode)
  :init
  (setq yas-snippet-dirs (concat emacs-root-path "snippets/"))
  :bind
  (("\t"      . yas-next-field-or-maybe-expand)
   ("M-k"     . yas-prev-field)
   :map yas-minor-mode-map
   ("C-c C-f" . yas-find-snippets))
  :config
  (yas-load-directory yas-snippet-dirs)
  (defun yas-find-snippets (&optional same-window)
    "Find snippet file in guessed current mode's directory.

Calls `find-file' interactively in the guessed directory.

With prefix arg SAME-WINDOW opens the buffer in the same window.

Because snippets can be loaded from many different locations,
this has to guess the correct directory using
`yas--guess-snippet-directories', which returns a list of
options.

If any one of these exists, it is taken and `find-file' is called
there, otherwise, proposes to create the first option returned by
-`yas--guess-snippet-directories'."
    (interactive "P")
    (let* ((guessed-directories (yas--guess-snippet-directories))
           (chosen)
           (buffer))
      (setq chosen (yas--make-directory-maybe (first guessed-directories) " main"))
      (unless chosen
        (if (y-or-n-p (format "Continue guessing for other active tables %s? "
                              (mapcar #'(lambda (table-and-dirs)
                                          (yas--table-name (car table-and-dirs)))
                                      (rest guessed-directories))))
            (setq chosen (some #'yas--make-directory-maybe
                               (rest guessed-directories)))))
      (unless chosen
        (when (y-or-n-p "Having trouble... go to snippet root dir? ")
          (setq chosen (first (yas/snippet-dirs)))))
      (if chosen
          (let ((default-directory chosen))
            (setq buffer (call-interactively (if same-window
                                                 'find-file
                                               'find-file-other-window)))
            (when buffer
              (save-excursion
                (set-buffer buffer)
                (when (eq major-mode 'fundamental-mode)
                  (snippet-mode)))))
        (message "Could not guess snippet dir!")))))

(yas-global-mode 1)

(provide 'yasnippet-settings)
;; yasnippet-settings ends here.
;;;
