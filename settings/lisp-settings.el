;; -*- Emacs-Lisp -*-
;; Last modified: <2016-08-29 11:51:13 Monday by richard>

;; Copyright (C) 2013 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1
;; PUBLIC LICENSE: GPLv3

;; Settings for clojure
;; -----------------------------------[Settings for clojure]
(add-to-list 'load-path (concat plugins-path-r "clojure-mode"))
(add-to-list 'load-path (concat plugins-path-r "cider"))
(add-to-list 'load-path (concat plugins-path-r "spinner.el"))
(add-to-list 'load-path (concat plugins-path-r "ac-cider"))

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

(use-package
  paredit
  :commands (paredit-mode)
  :bind (:map paredit-mode-map
              ("M-)" . paredit-forward-slurp-sexp)
              ("M-(" . paredit-backward-slurp-sexp)))

(use-package
  flycheck-clojure
  :defer t
  :after (flycheck cider)
  :config
  (flycheck-clojure-setup))

(use-package
  ac-cider
  :defer t
  :commands (ac-cider-setup ac-flyspell-workaround))

(use-package
  cider-macroexpansion
  :commands (cider-macroexpansion-mode))

(use-package
  cider
  :defer t
  :commands (cider-mode)
  :init
  (setq nrepl-hide-special-buffers t
        nrepl-buffer-name-show-port t
        cider-prefer-local-resources t
        cider-repl-display-in-current-window t
        cider-repl-history-size 10000
        cider-repl-result-prefix ";; => "
        cider-repl-history-file (concat emacs-root-path "cider-repl.history")
        cider-stacktrace-fill-column 80)
  :bind (:map cider-mode-map            ; bind keys in specific map
              ("C-c C-f" . yas-find-snippets)
              ("C-c g"   . cider-find-var))
  :config
  (add-hook 'nrepl-mode-hook 'ac-cider-setup)
  (add-hook 'nrepl-interaction-mode-hook 'ac-cider-setup)
  (add-hook 'nrepl-interaction-mode-hook 'eldoc-mode)
  (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
  (add-hook 'cider-mode-hook 'ac-cider-setup)
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'subword-mode)
  (add-hook 'cider-repl-mode-hook
            (lambda ()
              (setq dash-at-point-docset "clojure")
              (autoload 'dash-at-point "dash-at-point"
                "Search the word at point with Dash." t nil)
              (define-key cider-repl-mode-map (kbd "C-c d") 'dash-at-point))))

(use-package
  clojure-mode
  :config
  (add-hook 'clojure-mode-hook
            (lambda () (paredit-mode +1)
              (cider-mode +1)))
  :mode ("\.clj\'" . clojure-mode))

(eval-after-load "ob"
  ;; clojure integration for emacs
  '(progn
     (defun clojure-for-org-mode()

       (defvar org-babel-default-header-args:clojure
         '((:results . "silent") (:tangle . "yes")))

       (defun org-babel-execute:clojure (body params)
         "Evaluate a block of Clojure code with Babel."
         (lisp-eval-string body)
         "Done!"))
     (clojure-for-org-mode)))


(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'nrepl-mode)))

(dolist (hook '(emacs-lisp-mode-hook
                lisp-mode-hook
                lisp-interaction-mode-hook))
  (add-hook hook (lambda () (paredit-mode +1))))

;; Settings for scala
;; -------------------------------------[Settings for scala]
(add-to-list 'load-path (concat plugins-path-r "scala-mode2"))
;; (require 'scala-mode2)


;; Settings for elisp
;; -------------------------------------[Settings for elisp]
;; 1. Add eldoc for emacs
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(eval-after-load "eldoc"
  '(eldoc-add-command
    'paredit-backward-delete
    'paredit-close-round))


(provide 'lisp-settings)
;; lisp-settings ends here.
;;;
