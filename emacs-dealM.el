;; -*- Emacs-Lisp -*-
;; Last modified: <2012-07-26 12:51:50 Thursday by richard>

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
(defconst settings-path-r
  (concat emacs-root-path "settings/") "Personel prefer setting of lisp package")
(defconst template-path-r
  (concat emacs-root-path "templates/") "Path for template")

(setq default-directory emacs-root-path)

;; add load path
(add-to-list 'load-path settings-path-r)
(add-to-list 'load-path plugins-path-r)



;; tools
;; ------------------------------------------------------------------
(require 'git-settings)
(require 'yasnippet-settings)
(require 'unicode-settings)


;; Theme, color and fonts settings.
;; ------------------------------------------------------------------
(require 'theme-settings)
(require 'modeline-settings)
(require 'font-settings)
(require 'auto-color-settings)

(provide 'emacs-dealM)
;; emacs-dealM ends here.
;;;