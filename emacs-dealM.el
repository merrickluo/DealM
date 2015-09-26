;; -*- Emacs-Lisp -*-
;; Last modified: <2015-09-26 22:18:59 Saturday by wongrichard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1
;; PUBLIC LICENSE: FreeBSD

;; configure path depend on system.

(defconst emacs-root-path
  (file-name-directory (or load-file-name buffer-file-name)) "Emacs root settings path (in linux)")
(defconst plugins-path-r
  (concat emacs-root-path "plugins/") "Reference path of emacs lisp package")
(defconst themes-path-r
  (concat emacs-root-path "themes/") "Reference path of emacs theme")
(defconst settings-path-r
  (concat emacs-root-path "settings/") "Personel prefer setting of lisp package")
(defconst template-path-r
  (concat emacs-root-path "templates/") "Path for template")

(defcustom default-theme-r 'monokai
  "Default theme for dealM"
  :type 'symbol
  :group 'dealM)

;; add load path
(add-to-list 'load-path settings-path-r)
(add-to-list 'load-path plugins-path-r)
(add-to-list 'load-path (concat plugins-path-r "dash/"))


;; tools
;; ------------------------------------------------------------------
(require 'git-settings)
(require 'yasnippet-settings)
(require 'unicode-settings)
(require 'command-frequence)
(require 'auto-complete-setting)
(require 'flymake-settings)
(require 'compile-settings)
(require 'auto-insert-settings)
(require 'autopair-settings)


;; Theme, color and fonts, encoding settings.
;; ------------------------------------------------------------------
(require 'theme-settings)
(require 'modeline-settings)
(require 'font-settings)
(require 'auto-color-settings)


;; default emacs behavior settings(keys and common settings)
;; ------------------------------------------------------------------
(require 'default-behavior-settings)
(require 'window-buffer-settings)
(require 'edit-settings)
(require 'ido-settings)
(require 'dired-settings)


;; Programming settings
;; ------------------------------------------------------------------
(require 'dev-settings)
(require 'webdev-settings)
(require 'python-settings)
(require 'lisp-settings)
(require 'molo-settings)

;; desktop settings
;; (require 'desktop-settings)


;; registers.
;; ------------------------------------------------------------------
(set-register ?e '(file . "~/.emacs"))               ;; C-x r j e: register jump to the .emacs(home).
(set-register ?w '(file . "~/.words.org"))           ;; C-x r j w: register jump to words.
(set-register ?t '(file . "/tmp/test.py"))           ;; C-x r j t: goto test python file.

(provide 'emacs-dealM)
;; emacs-dealM ends here.
;;;
