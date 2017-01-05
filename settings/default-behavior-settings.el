;; -*- Emacs-Lisp -*-
;; Last modified: <2017-01-05 17:41:38 Thursday by merrick>

;; Copyright (C) 2012-2013 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.5
;; PUBLIC LICENSE: GPLv3

;; Emacs default behavoir settings

;; Personal Info

(setq change-log-default-name   "ChangeLog"
      user-full-name            "Richard Wong"
      user-nick-name            "@richard"
      major-mode                'text-mode
      enable-local-variables    :safe
      log-mailing-address       "log@cccc.com"
      ;; My Gmail address, Welcome letter
      user-mail-address         "chao787@gmail.com")

(setq-default default-directory "~"
              indicate-buffer-boundaries 'left)

(setq line-move-visual nil
      visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))


(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta))

;; hide menu-bar and tool-bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))

;; speed up keystroke
(setq echo-keystrokes 0.1
      font-lock-maximum-decoration t
      system-time-locale "C")

(global-font-lock-mode t)

;; disable scroll bar
(customize-set-variable 'scroll-bar-mode nil)

;;; Unbind the stupid minimize that I always hit.
(global-unset-key "\C-z")


(use-package files
  :init
  ;; Diable backup
  (setq-default make-backup-files nil
                auto-save-default nil))

(use-package tramp-sh
  :defer t
  :config
  (add-to-list 'tramp-remote-process-environment "LC_ALL=zh_CN.utf8" 'append)
  (add-to-list 'tramp-remote-process-environment "LANG=zh_CN.utf8" 'append))

(use-package tramp
  :defer t
  :commands (tramp-mode)
  :init
  (setq tramp-default-method "ssh"
        ido-enable-tramp-completion t
        ;; workaround for tramp, see: http://goo.gl/DUKMC8
        tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no"
        tramp-verbose 4                 ; 1 - 10
        tramp-persistency-file-name
        (concat emacs-root-path ".auto-save-list-tramp")))

(add-hook 'text-mode-hook 'text-mode-hook-identify)
(add-hook 'text-mode-hook 'turn-off-auto-fill)

;; insert two spaces after two colon:
(setq colon-double-space t
      column-number-mode t)

;; emacs lock
(autoload 'toggle-emacs-lock "emacs-lock" "Emacs lock" t)

;; 启用以下功能
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
;; disable cursor blinking
(blink-cursor-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)

;; disable emacs start screen
(setq inhibit-startup-message t
      initial-scratch-message "")



;; 简写模式
(setq-default abbrev-mode t)
(setq save-abbrevs nil)

;; 防止页面滚动时跳动,scroll-margin 3可以在靠近屏幕边沿3行时就开始滚动,可以很好的看到上下文
(setq scroll-margin 3
      scroll-conservatively 10000
      ;; enable-recursive-minibuffers t
      ring-bell-function 'ignore
      ;; Automatically add newlines in last of file. (Enhancing C-n)
      next-line-add-newlines t
      require-final-newline t)

;; 当你在shell、telnet、w3m等模式下时，必然碰到过要输入密码的情况,此时加密显出你的密码
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;; 可以保存你上次光标所在的位置
(save-place-mode t)

;; 光标靠近鼠标指针时，让鼠标指针自动让开，别挡住视线。
(mouse-avoidance-mode 'animate)

;; kill ring settings
(setq kill-do-not-save-duplicates t
      kill-ring-max 200
      ;; format then auto complete
      tab-always-indent 'complete)


;; 缩进设置
;; 不用TAB字符来indent
;; TAB 宽度设置为4
(setq-default indent-tabs-mode nil)
(setq tab-width 4
      tab-stop-list nil)

(loop for x downfrom 40 to 1 do
      (setq tab-stop-list (cons (* x tab-width) tab-stop-list)))

;; More generic highlight settings.
(require 'generic-x)
;; time-stamp settings. Change modified style
(add-hook 'write-file-hooks 'time-stamp)
(setq time-stamp-start "Last modified:[ \t]+\\\\?[\"<]+"
      time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S %:a by %u"
      time-stamp-end "\\\\?[\">]")

;; set basic cua-mode.
(setq cua-remap-control-z nil
      cua-remap-control-v nil)

;; Encoding settings
(when (not window-system)
  (set-language-environment "UTF-8"))

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq buffer-file-coding-system 'utf-8)

(use-package log4j-mode
  :defer t
  :commands
  (log4j-mode)
  :init
  (setq log4j-keyword-fatal "^\\[C.*\\]\\|\\<\\(FATAL\\|CRITICAL\\)\\>"
        log4j-keyword-error "^\\[E.*\\]\\|\\<\\(ERROR\\|SEVERE\\|\\[E.*\\]\\)\\>"
        log4j-keyword-warn  "^\\[W.*\\]\\|\\<\\(WARN\\(?:ING\\)?\\|\\[W.*\\]\\)\\>"
        log4j-keyword-debug "^\\[D.*\\]\\|\\<\\(DEBUG\\|FINE\\(?:R\\|ST\\)?\\|STATUS\\)\\>"
        log4j-keyword-info  "^\\[I.*\\]\\|\\<\\(CONFIG\\|INFO\\)\\>")
  :config
  (autoload 'itail-mode "itail" "" t)
  (add-hook 'log4j-mode-hook #'(lambda () (itail-mode)))
  :mode ("\\.[Ll][Oo][Gg]$" . log4j-mode))

(autoload 'xahk-mode "xahk-mode" "" t)

(autoload 'coffee-mode "coffee-mode" "" t)

(autoload 'dockerfile-mode "dockerfile-mode" "" t)

(autoload 'cython-mode "cython-mode" "" t)

(add-to-list 'load-path (concat plugins-path-r "rust-mode/"))
(autoload 'rust-mode "rust-mode" "" t)
(autoload 'json-mode "json-mode" "" t)
(autoload 'php-mode "php-mode" "" t)
(autoload 'cmake-mode "cmake-mode" "" t)
(autoload 'typescript-mode "typescript" "typescript mode for tss" t)
(autoload 'adoc-mode "adoc-mode" "" t)

;; modes definition.
(setq auto-mode-alist
      (append '(("\\.[Cc][Xx][Xx]$" . c++-mode)
                ("\\.[Cc][Pp][Pp]$" . c++-mode)
                ("\\.[Hh][Xx][Xx]$" . c++-mode)
                ("\\.[Tt][Cc][Cc]$" . c++-mode)
                ("\\.coffee$" . coffee-mode)
                ("\\.[Yy][Aa]?[Mm][Ll]$" . yaml-mode)
                ("\\.[Cc][Uu][Hh]?$" . cuda-mode)
                ("\\.sass$" . sass-mode)
                ("\\.scss$" . scss-mode)
                ("\\.less\\'" . less-css-mode)
                ("\\.h$" . c++-mode)
                ("\\.i$" . c++-mode)    ; SWIG
                ("Dockerfile\\'" . dockerfile-mode)
                ;; ("\\.m$" . octave-mode)
                ("\\.m$" . objc-mode)
                ("\\.mm$" . objc-mode)
                ("_emacs" . lisp-mode)
                ("\\.el\\.gz$" . emacs-lisp-mode)
                ("\\.mak$" . makefile-mode)
                ("\\.conf$" . conf-mode)
                ("\\.go$" .  go-mode)
                ("Doxyfile.tmpl$" . makefile-mode)
                ("Doxyfile$" . makefile-mode)
                ("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode)
                ("\\.uncompressed$" . hexl-mode)
                ("\\.js$" . js2-mode)
                ("\\.ts$" . typescript-mode)
                ("\\.clj$" . clojure-mode)
                ("\\.json$" . json-mode)
                ("\\.y[s]?$" . snippet-mode)
                ("headx$" . snippet-mode)
                ("\\.ke$" . kepago-mode)
                ("\\.asciidoc$" . adoc-mode)
                ("\\.txt$" . adoc-mode)
                ("\\.[Aa][Hh][Kk]$" . xahk-mode)
                ("\\.[Pp][Hh][Pp]$" . php-mode)
                ("\\.textile$" . textile-mode)
                ("\\.kfn$" . kfn-mode)
                ("Podfile\\'" . ruby-mode)
                ("\\.podspec\\'" . ruby-mode)
                ("\\.rb$" . ruby-mode)
                ("\\.cml$" . xml-mode)
                ("\\.cg$" . cg-mode)
                ("\\.yy$" . bison-mode)
                ("\\.l[l]?$" . flex-mode)
                ("\\.lua$" . lua-mode)
                ("\\.org$" . org-mode)
                ("\\.rs\\'" . rust-mode)
                ("\\.\\(todo\\|do\\|plan\\)$". org-mode)
                ("\\.scons$" . python-mode)
                ("\\.pyx$" . cython-mode)
                ("SCons\\(cript\\|truct\\)" . python-mode)
                ("\\.gclient$" . python-mode)
                ) auto-mode-alist))

;; markdown-settings
(use-package markdown-mode
  :mode
  ("\\.markdown\\'" . markdown-mode)
  ("\\.md\\'" . markdown-mode)
  ("\\.text\\'" . markdown-mode))

;; learning from dadams
(eval-after-load "ring"
  '(progn (require 'ring+)))

(when (string= system-type "darwin")
;;; ENV path correction for (Mac os x)
  (dolist (ensure-path '("/usr/bin"
                         "/bin"
                         "/usr/sbin"
                         "/sbin"
                         "/Developer/NVIDIA/CUDA-5.5/bin"
                         "/usr/local/share/python"
                         "/usr/local/bin"))
    (unless (member ensure-path exec-path)
      (add-to-list 'exec-path ensure-path)))
  (setenv "PATH" (mapconcat 'identity exec-path ":"))
  (setenv "LC_ALL" "en_US.UTF-8")
  (setenv "LC_CTYPE" "en_US.UTF-8"))

(provide 'default-behavior-settings)
;; default-behavior-settings ends here.
;;;
