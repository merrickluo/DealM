;; -*- Emacs-Lisp -*-
;; Last modified: <2016-09-20 18:05:08 Tuesday by richard>

;; Copyright (C) 2012-2015 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.2
;; PUBLIC LICENSE: FreeBSD

;; configure path depend on system.

;; workaround for tramp, see: http://goo.gl/DUKMC8
(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
(require 'tramp)

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

(defcustom default-source-font-r "Courier-14"
  "Default font for dealM"
  :type 'string
  :group 'dealM)

(set-face-attribute 'default nil :font default-source-font-r)

;; add load path
(add-to-list 'load-path settings-path-r)
(add-to-list 'load-path plugins-path-r)
(add-to-list 'load-path (concat plugins-path-r "dash/"))
(add-to-list 'load-path (concat plugins-path-r "use-package/"))


;; tools
;; ------------------------------------------------------------------
(require 'use-package)
(require 'flycheck-settings)
(require 'yasnippet-settings)
(require 'unicode-settings)
(require 'command-frequence)
(require 'auto-complete-setting)
(require 'compile-settings)
(require 'auto-insert-settings)


;; Theme, color and fonts, encoding settings.
;; ------------------------------------------------------------------
(require 'theme-settings)
(require 'modeline-settings)
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
(require 'org-settings)

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
