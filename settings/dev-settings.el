;; -*- Emacs-Lisp -*-
;; Last modified: <2012-08-06 17:42:02 Monday by richard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1
;; PUBLIC LICENSE: GPLv3
;; This file is a extension for edit-settings.

;; autoloads
;; ------------------------------------------------------------------
(autoload 'highlight-indentation-mode "highlight-indentation" "")

(eval-after-load "highlight-indentation"
  '(progn
     (set-face-background 'highlight-indentation-face "grey30")
     (set-face-background 'highlight-indentation-current-column-face "grey50")
     ))

;; show-wspace settings.
;; ------------------------------------------------------------------
(autoload 'toggle-highlight-tabs "show-wspace" "" t)
(autoload 'toggle-highlight-hard-spaces "show-wspace" "" t)
(autoload 'toggle-highlight-other-chars "show-wspace" "" t)
(autoload 'toggle-highlight-trailing-whitespace "show-wspace" "" t)

;; watch specific word color
;; ------------------------------------------------------------------
(defun esk-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))


;; Find-things-fast settings.
;; ------------------------------------------------------------------
(add-to-list 'load-path (concat plugins-path-r "find-things-fast"))
(autoload 'ftf-grepsource    "find-things-fast" "" t)
(autoload 'ftf-find-file     "find-things-fast" "" t)
(autoload 'ftf-compile       "find-things-fast" "" t)
(autoload 'ftf-gdb           "find-things-fast" "" t)
(autoload 'ftf-add-filetypes "find-things-fast")

(global-set-key '[f1] 'ftf-find-file)
(global-set-key '[f2] 'ftf-grepsource)
(define-key dired-mode-map '[f1] 'ftf-find-file)


;; parenthses settings
;; ------------------------------------------------------------------
(autoload 'highlight-parentheses-mode "highlight-parentheses")

(setq hl-paren-colors '("red" "yellow" "cyan" "magenta" "green" "red"))

(setq paren-message-show-linenumber 'absolute)
(autoload 'paren-activate                         "mic-paren" "" t)
(autoload 'paren-deactivate                       "mic-paren" "" t)
(autoload 'paren-toggle-matching-paired-delimiter "mic-paren" "" t)
(autoload 'paren-toggle-matching-quoted-paren     "mic-paren" "" t)
(autoload 'paren-toggle-open-paren-context        "mic-paren" "" t)
(show-paren-mode t)



;;----------------------------------------------------------
;; Mode specific shortcut settings.
;;----------------------------------------------------------
(defun start-program-short-cut()
  "common program short-cut keys."
  ;; RET is reindent thisline and indent the new line.
  (local-set-key (kbd "RET")     'reindent-then-newline-and-indent)
  (local-set-key "\C-k"          'program-smart-kill)
  (local-set-key (kbd "C-c RET") 'compile-buffer)
  (local-set-key (kbd "C-c C-c") 'comment)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace)
  (linum-mode t)
  ;; highlight settings by mode.
  (hl-line-mode t)
  (toggle-highlight-trailing-whitespace)
  (toggle-highlight-tabs)
  (toggle-highlight-hard-spaces)
  (toggle-highlight-other-chars)
  (highlight-parentheses-mode t)
  (highlight-indentation-mode t)
  (esk-add-watchwords)
  )

;; lisp short cut Settings.
;; ==================================================================
(defun lisp-short-cut()
  "Lisp Specific mode short-cut key settings."
  (start-program-short-cut)

  (ftf-add-filetypes '("*.el" "*.elisp"))

  (local-set-key (kbd "C-c C-k") 'kill-function)
  (local-set-key (kbd "C-M-h")   'mark-function)
  (local-set-key (kbd "C-c D")   'edebug-defun)
  (local-set-key (kbd "C-c C-d") 'eval-defun)
  (local-set-key (kbd "C-c B")   'eval-buffer)
  (local-set-key (kbd "C-c M-w") 'copy-function-whole)
  (local-set-key (kbd "C-c C-q") 'indent-function)
  (local-set-key (kbd "C-c C")   'comment-function))

;; c-common-mode short cut settings.
;; ==================================================================
(defun c-common-short-cut()
  "c common mode short-cut key settings."
  (start-program-short-cut)
  (smart-operator-mode-on)

  (paren-toggle-open-paren-context 1)
  ;; Cedet settings.
  ;; (require 'cedet-settings)
  (local-set-key (kbd "C-c C-k") 'kill-function)
  (local-set-key (kbd "C-c M-w") 'copy-function-whole)
  (local-set-key (kbd "C-M-h") 'mark-function)
  (local-set-key (kbd "C-c C") 'comment-function))



;; python mode short cut settings.
;; ==================================================================
(defun python-short-cut()
  "python mode short-cut key settings."
  (smart-operator-mode-on)
  (start-program-short-cut)
  ;; new speacial complete. hijhij
  (local-set-key (kbd "M-?") 'rope-code-assist)
  (local-set-key (kbd "M-/") 'rope-lucky-assist)
  (local-set-key (kbd "<f12>") 'rope-goto-definition)
  (local-set-key (kbd "C-c d") 'rope-show-doc)
  (local-set-key (kbd "C-c f") 'rope-find-occurrences)

  ;; Rope bindings
  (add-hook 'python-mode-hook
            (local-set-key "\C-ci" 'rope-auto-import)
            (local-set-key "\C-c\C-d" 'rope-show-calltip)))

;; awk mode short cut settings.
;; ==================================================================
(defun awk-short-cut()
  "awk mode short-cut key settings."
  (smart-operator-mode-on)
  (start-program-short-cut))

;; shell mode short cut settings.
;; ==================================================================
(defun shell-short-cut()
  "shell mode short-cut key settings."
  (smart-operator-mode-on)
  (local-set-key (kbd "<")       'self-insert-command)
  (local-set-key (kbd "C-c M-c") 'sh-case)
  (local-set-key (kbd "C-c g")   'bashdb))

;; tex mode short cut settings.
;; ==================================================================
(defun tex-short-cut()
  "shell mode short-cut key settings."
  (start-program-short-cut)
  ;; compatible with flyspell.

  (paren-toggle-matching-quoted-paren 1)
  (paren-toggle-matching-paired-delimiter 1)
  (ac-flyspell-workaround))

;; js2-mode settings
;; ==================================================================
(defun js2-short-cut()
  "js2 mode short-cut key settings."
  (start-program-short-cut)
  ;; compatible with flyspell.
  (smart-operator-mode-on))


;; Short cut Hooks here.
;; ==================================================================
(add-hook 'emacs-lisp-mode-hook 'lisp-short-cut)
(add-hook 'c-mode-common-hook   'c-common-short-cut)
(add-hook 'python-mode-hook     'python-short-cut)
(add-hook 'awk-mode-hook        'awk-short-cut);; After emacs 21 work here.
(add-hook 'shell-mode-hook      'shell-short-cut)
(add-hook 'LaTex-mode-hook      'tex-short-cut)
(add-hook 'js2-mode-hook        'js2-short-cut)

(provide 'dev-settings)
;; dev-settings ends here.
;;;
