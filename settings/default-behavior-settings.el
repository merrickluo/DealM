;; -*- Emacs-Lisp -*-
;; Last modified: <2012-11-13 08:50:45 Tuesday by June>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.5
;; PUBLIC LICENSE: GPLv3

;; Emacs default behavoir settings

;; Personal Info

(setq change-log-default-name   "ChangeLog"
      user-full-name            "Richard Wong"
      major-mode                'text-mode
      enable-local-variables    :safe
      log-mailing-address       "chao78787@gmail.com"
      ;; My Gmail address, Welcome letter
      user-mail-address         "chao787@gmail.com")

(setq-default default-directory "~")

;; 在fringe上显示一个小箭头指示当前buffer的边界
(setq-default indicate-buffer-boundaries 'left)

;; hide menu-bar and tool-bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))

;; 尽快显示按键序列
(setq echo-keystrokes 0.1)

;; Font lock...
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(setq system-time-locale "C")

;; 不要滚动条
(customize-set-variable 'scroll-bar-mode nil)

;;; Unbind the stupid minimize that I always hit.
(global-unset-key "\C-z")

;; 支持emacs和外部程序的粘贴
;; emacs 24 default
;; (setq x-select-enable-clipboard t)

;; 不产生备份文件
(setq-default make-backup-files nil)

(add-hook 'text-mode-hook 'text-mode-hook-identify)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; insert two spaces after two colon:
(setq colon-double-space t)

;; 显示列号
(setq column-number-mode t)

;; emacs lock
(autoload 'toggle-emacs-lock "emacs-lock" "Emacs lock" t)

;; 启用以下功能
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; 不显示Emacs的开始画面
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; 不要总是没完没了的问yes or no, 为什么不能用y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; 不要闪烁光标, 烦不烦啊
(blink-cursor-mode -1)

;; 简写模式
(setq-default abbrev-mode t)
(setq save-abbrevs nil)

;; 防止页面滚动时跳动,scroll-margin 3可以在靠近屏幕边沿3行时就开始滚动,可以很好的看到上下文
(setq scroll-margin 3
      scroll-conservatively 10000)

;; 没有提示音,也不闪屏
(setq ring-bell-function 'ignore)

;; 可以递归的使用minibuffer
;; (setq enable-recursive-minibuffers t)

;; 当你在shell、telnet、w3m等模式下时，必然碰到过要输入密码的情况,此时加密显出你的密码
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;; Automatically add newlines in last of file. (Enhancing C-n)
(setq next-line-add-newlines t)

;; 可以保存你上次光标所在的位置
;; (require 'saveplace)
(setq-default save-place t)

;; 光标靠近鼠标指针时，让鼠标指针自动让开，别挡住视线。
(mouse-avoidance-mode 'animate)

;; 不保存连续的重复的kill
(setq kill-do-not-save-duplicates t)
;; 用一个很大的kill ring. 这样防止我不小心删掉重要的东西
(setq kill-ring-max 200)
;; 先格式化再补全
(setq tab-always-indent 'complete)


;; 缩进设置
;; 不用TAB字符来indent
;; TAB 宽度设置为4
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq tab-stop-list nil)
(loop for x downfrom 40 to 1 do
      (setq tab-stop-list (cons (* x tab-width) tab-stop-list)))
;; Automatically add a line in the end of file
(setq require-final-newline t)

;; More generic highlight settings.
(require 'generic-x)
;; time-stamp settings. Change modified style
(add-hook 'write-file-hooks 'time-stamp)
(setq time-stamp-start "Last modified:[ \t]+\\\\?[\"<]+")
(setq time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S %:a by %u")
(setq time-stamp-end "\\\\?[\">]")

;; log mode to replace fundamentals
(autoload 'log4j-mode "log4j-mode" "" t )
;; settings for log4j-mode
(setq log4j-keyword-fatal "^\\[C.*\\]\\|\\<\\(FATAL\\|CRITICAL\\)\\>"
      log4j-keyword-error "^\\[E.*\\]\\|\\<\\(ERROR\\|SEVERE\\|\\[E.*\\]\\)\\>"
      log4j-keyword-warn  "^\\[W.*\\]\\|\\<\\(WARN\\(?:ING\\)?\\|\\[W.*\\]\\)\\>"
      log4j-keyword-debug "^\\[D.*\\]\\|\\<\\(DEBUG\\|FINE\\(?:R\\|ST\\)?\\|STATUS\\)\\>"
      log4j-keyword-info  "^\\[I.*\\]\\|\\<\\(CONFIG\\|INFO\\)\\>")

(autoload 'xahk-mode "xahk-mode" "" t)
;; modes definition.
(setq auto-mode-alist
      (append '(("\\.[Cc][Xx][Xx]$" . c++-mode)
                ("\\.[Cc][Pp][Pp]$" . c++-mode)
                ("\\.[Hh][Xx][Xx]$" . c++-mode)
                ("\\.[Tt][Cc][Cc]$" . c++-mode)
                ("\\.[Ll][Oo][Gg]$" . log4j-mode)
                ("\\.h$" . c++-mode)
                ("\\.i$" . c++-mode)    ; SWIG
                ("\\.mm?$" . objc-mode)
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
                ("\\.json$" . js2-mode)
                ("\\.y[s]?$" . snippet-mode)
                ("headx$" . snippet-mode)
                ("\\.ke$" . kepago-mode)
                ("\\.markdown$" . markdown-mode)
                ("\\.md$" . markdown-mode)
                ("\\.[Aa][Hh][Kk]$" . xahk-mode)
                ("\\.textile$" . textile-mode)
                ("\\.kfn$" . kfn-mode)
                ("\\.rb$" . ruby-mode)
                ("\\.cml$" . xml-mode)
                ("\\.cg$" . cg-mode)
                ("\\.yy$" . bison-mode)
                ("\\.l[l]?$" . flex-mode)
                ("\\.lua$" . lua-mode)
                ("\\.org$" . org-mode)
                ("\\.\\(todo\\|do\\|plan\\)$". org-mode)
                ("\\.scons$" . python-mode)
                ("SCons\\(cript\\|truct\\)" . python-mode)
                ("\\.gclient$" . python-mode)
                ) auto-mode-alist))

;; learning from dadams
(eval-after-load "ring"
  '(progn (require 'ring+)))

(eval-after-load "thingatpt"
  '(require 'thingatpt+))

(eval-after-load "grep"
  '(require 'grep+))

(eval-after-load "window"
  '(require 'window+))

(eval-after-load "help"
  '(require 'help+))

(eval-after-load "help-fns"
  '(require 'help-fns+))

(eval-after-load "info"
  '(require 'info+))

(eval-after-load "mouse"
  '(require 'mouse+))

(eval-after-load "bookmark"
  '(require 'bookmark+))

(require 'simple+)

(provide 'default-behavior-settings)
;; default-behavior-settings ends here.
;;;
