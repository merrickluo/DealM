;; -*- Emacs-Lisp -*-
;; Last modified: <2016-01-03 10:27:35 Sunday by wongrichard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1
;; PUBLIC LICENSE: GPLv3
;; This file is a extension for edit-settings.

;; autoloads
;; ------------------------------------------------------------------
(autoload 'highlight-indentation-mode "highlight-indentation" "")
(autoload 'cuda-mode "cuda-mode" "")
(autoload 'lua-mode "lua-mode" "")
(autoload 'yaml-mode "yaml-mode" "")
(autoload 'electric-spacing-mode "electric-spacing" "\
Insert operators with surrounding spaces smartly.

\(fn &optional ARG)" t nil)

(add-to-list 'auto-mode-alist '("\\.cuh?\\'" . cuda-mode))

(require 'sr-speedbar)
(eval-after-load "highlight-indentation"
  '(progn
     (set-face-background 'highlight-indentation-face "grey30")
     (set-face-background 'highlight-indentation-current-column-face "grey50")
     ))


;; global modes
;; ------------------------------------------------------------------
(electric-pair-mode)


;; hl-line+ settings.
;; ------------------------------------------------------------------

(defface hl-line '((t (:background "SlateGray3"))) "\
Face to use for `hl-line-face'." :group (quote hl-line))

(defvar hl-line-flash-show-period 1 "\
Number of seconds for `hl-line-flash' to highlight the line.")

(custom-autoload 'hl-line-flash-show-period "hl-line+" t)

(defvar hl-line-inhibit-highlighting-for-modes nil "\
Modes where highlighting is inhibited for `hl-line-highlight-now'.
A list of `major-mode' values (symbols).")

(custom-autoload 'hl-line-inhibit-highlighting-for-modes "hl-line+" t)

(defalias 'toggle-hl-line-when-idle 'hl-line-toggle-when-idle)

(autoload 'hl-line-toggle-when-idle "hl-line+" "\
Turn on or off using `global-hl-line-mode' when Emacs is idle.
When on, use `global-hl-line-mode' whenever Emacs is idle.
With prefix argument, turn on if ARG > 0; else turn off.

\(fn &optional ARG)" t nil)

(autoload 'hl-line-when-idle-interval "hl-line+" "\
Set wait until using `global-hl-line-mode' when Emacs is idle.
Whenever Emacs is idle for this many seconds, `global-hl-line-mode'
will be turned on.

To turn on or off using `global-hl-line-mode' when idle,
use `\\[toggle-hl-line-when-idle].

\(fn SECS)" t nil)

(defalias 'flash-line-highlight 'hl-line-flash)

(autoload 'hl-line-flash "hl-line+" "\
Highlight the current line for `hl-line-flash-show-period' seconds.
With a prefix argument, highlight for that many seconds.

\(fn &optional ARG)" t nil)


;; highlight-symbol settings.
;; ------------------------------------------------------------------
(autoload 'global-auto-highlight-symbol-mode "auto-highlight-symbol" "" t)
(autoload 'auto-highlight-symbol-mode "auto-highlight-symbol" "" t)
(autoload 'highlight-symbol-at-point "highlight-symbol" "" t)
(defalias 'highlight-symbol 'highlight-symbol-at-point)


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
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\|WARN\\(ING\\)?\\)"
          1 font-lock-warning-face t))))


;; Go-mode settings. (go programming language)
;; ------------------------------------------------------------------
(autoload 'go-mode "go-mode" "\
Major mode for editing Go source text.

This provides basic syntax highlighting for keywords, built-ins,
functions, and some types.  It also provides indentation that is
\(almost) identical to gofmt.

\(fn)" t nil)

(autoload 'gofmt "go-mode" "\
Pipe the current buffer through the external tool `gofmt`.
Replace the current buffer on success; display errors on failure.

\(fn)" t nil)

(autoload 'gofmt-before-save "go-mode" "\
Add this to .emacs to run gofmt on the current buffer when saving:
 (add-hook 'before-save-hook #'gofmt-before-save)

\(fn)" t nil)




;; Projectile settings. more smarter than ftf
;; ------------------------------------------------------------------
(require 'projectile)
(defun smart-find-file ()
  (interactive)
  (if (projectile-project-p)
      (call-interactively 'projectile-find-file)
    (call-interactively 'ido-find-file)))

(defun feeling-lucky-grep (pattern)
  (interactive
   (list (read-string "git grep: "
                      (shell-quote-argument (grep-tag-default)))))
  (require 'magit)
  (with-current-buffer (generate-new-buffer "*Magit Grep*")
    (setq default-directory (projectile-project-root))
    (insert magit-git-executable " "
            (mapconcat 'identity magit-git-standard-options " ")
            " grep -n "
            (shell-quote-argument pattern) "\n\n")
    (magit-git-insert "grep" "--line-number" "--color" pattern)
    (ansi-color-apply-on-region (point-min) (point-max))
    ;; probably need to change the order of these two.
    (grep-mode)
    (pop-to-buffer (current-buffer))
    )
  ;; Am I lucky here?
  ;; (when (= (count-lines (point-min) (point-max)) 3)
  ;;   (let* ((cf (current-window-configuration))
  ;;          (grep-buffer (current-buffer))
  ;;          (target-buffer (progn (first-error) (current-buffer))))
  ;;     (message "killed buffer")
  ;;     (kill-buffer (current-buffer))
  ;;     (set-window-configuration cf)
  ;;     (replace-buffer-in-windows target-buffer)
  ;;     ))
  )

(defun smart-grep (&optional arg)
  (interactive "p")
  (or arg (setq arg 1))
  (if (string-equal (projectile-project-vcs) "git")
      (if (= arg 1)
          (call-interactively 'feeling-lucky-grep)
        (call-interactively 'vc-git-grep))
    (call-interactively 'projectile-grep)))

(global-set-key '[f1] 'smart-find-file)
(global-set-key '[f2] 'smart-grep)

(eval-after-load "dired"
  '(define-key dired-mode-map '[f1] 'smart-find-file))


;; parenthses settings
;; ------------------------------------------------------------------
(add-to-list 'load-path (concat plugins-path-r "rainbow-delimiters/"))
(autoload 'rainbow-delimiters-mode "rainbow-delimiters.el" "" t)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(setq rainbow-delimiters-unmatched-face)
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
  (auto-highlight-symbol-mode)
  ;; (toggle-highlight-trailing-whitespace)
  ;; (toggle-highlight-tabs)
  ;; (toggle-highlight-hard-spaces)
  ;; (toggle-highlight-other-chars)
  (highlight-parentheses-mode t)
  (highlight-indentation-mode t)
  (esk-add-watchwords)
  (autoload 'dash-at-point "dash-at-point"
    "Search the word at point with Dash." t nil)
  (local-set-key (kbd "C-c d") 'dash-at-point)
  )

;; lisp short cut Settings.
;; ==================================================================
(defun elisp-short-cut()
  "Lisp Specific mode short-cut key settings."
  (start-program-short-cut)

  (local-set-key (kbd "C-c C-k") 'kill-function)
  (local-set-key (kbd "C-M-h")   'mark-function)
  (local-set-key (kbd "C-c D")   'edebug-defun)
  (local-set-key (kbd "C-c C-d") 'eval-defun)
  (local-set-key (kbd "C-c B")   'eval-buffer)
  (local-set-key (kbd "C-c M-w") 'copy-function-whole)
  (local-set-key (kbd "C-c C-q") 'indent-function)
  (local-set-key (kbd "C-c C")   'comment-function))

;; typescript short cut Settings.
;; ==================================================================
(defun typescript-short-cut()
  "Type Script mode short-cut key settings."
  (start-program-short-cut)
  (require 'tss)

  ;; Key binding
  (setq tss-popup-help-key "C-:")
  (setq tss-jump-to-definition-key "C-c g")
  (setq tss-implement-definition-key "C-c i")

  ;; Make config suit for you. About the config item, eval the following sexp.
  ;; (customize-group "tss")

  ;; Do setting recommemded configuration
  (tss-config-default))

;; json short cut Settings.
;; ==================================================================
(defun json-short-cut()
  "json mode short-cut key settings."
  (start-program-short-cut)
  (setq json-reformat:indent-width 2)
  (defun json-smart-indent ()
    (interactive)
    (save-excursion
      (unless mark-active
        (call-interactively 'mark-whole-buffer))
      (call-interactively 'json-reformat-region)))
  (local-set-key (kbd "C-x TAB") 'json-smart-indent)
  )

;; clojure short cut Settings.
;; ==================================================================
(defun clojure-short-cut()
  "Clojure mode short-cut key settings."
  (start-program-short-cut)

  (local-set-key (kbd "C-c C")   'comment-function))

;; c-common-mode short cut settings.
;; ==================================================================
(defun c-common-short-cut()
  "c common mode short-cut key settings."
  (start-program-short-cut)
  (electric-spacing-mode)
  (setq c-basic-offset 2)
  (paren-toggle-open-paren-context 1)
  (local-set-key (kbd "C-c C-k") 'kill-function)
  (local-set-key (kbd "C-c M-w") 'copy-function-whole)
  (local-set-key (kbd "C-M-h") 'mark-function)
  (local-set-key (kbd "C-c C") 'comment-function))

;; object c short cut settings.
;; ==================================================================
(defun objc-short-cut()
  "object C short-cut key settings."
  (highlight-indentation-mode nil))

;; python mode short cut settings.
;; ==================================================================
(defun python-short-cut()
  "python mode short-cut key settings."
  (electric-spacing-mode)
  (start-program-short-cut)
  (local-set-key (kbd "C-c g") 'jedi:goto-definition)
  (local-set-key (kbd "M-/") 'jedi:complete)
  (local-set-key (kbd "C-c d") 'jedi:show-doc))

;; awk mode short cut settings.
;; ==================================================================
(defun awk-short-cut()
  "awk mode short-cut key settings."
  (electric-spacing-mode)
  (start-program-short-cut))

;; shell mode short cut settings.
;; ==================================================================
(defun shell-short-cut()
  "shell mode short-cut key settings."
  (start-program-short-cut)
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

;; Jade-Mode settings
;; ==================================================================
(add-to-list 'load-path (concat plugins-path-r "jade-mode/"))

(autoload 'sws-mode "sws-mode" "" t)
(autoload 'jade-mode "jade-mode" "" t)

(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

(defun jade-short-cut()
  (start-program-short-cut)
  )


;; js2-mode settings
;; ==================================================================
(autoload 'js2-mode "js2-mode" "\
Major mode for editing JavaScript code.

\(fn)" t nil)
(setq js2-mirror-mode nil)

(defun js2-short-cut()

  "js2 mode short-cut key settings."
  (start-program-short-cut)
  (setq js2-basic-offset 2)             ; continuation line indent to 4
  ;; compatible with flyspell.
  (electric-spacing-mode))



(defun rust-short-cut()
  "js2 mode short-cut key settings."
  (defun cargo-run()
    (interactive)
    (compile (format "cargo run"))
    )
  (defun cargo-test()
    (interactive)
    ;; TODO make it support C-u to test all project
    (compile (format "cargo test"))
    )
  (defun cargo-valgrind()
    (interactive)
    ;; TODO make it support C-u to test all project

    (projectile-with-default-dir (projectile-project-root)
      (shell-command (concat "valgrind target/debug/"
                             (projectile-project-name))))
    )

  (defun rust-electric-pair-inhibit-predicate-wrap-r(char)
    "Wraps the default `electric-pair-inhibit-predicate' to prevent
  inserting a \"matching\" > after a < that would be treated as a
  less than sign rather than as an opening angle bracket."
    (cond ((= ?> char)
           (message "wat >" char)
           t)
          ((= ?< char)
           t)
          ((= ?{ char)
           t)
          (t (funcall (default-value 'electric-pair-inhibit-predicate) char)))
    )
  (setq-local electric-pair-inhibit-predicate
              'rust-electric-pair-inhibit-predicate-wrap-r)

  (message "loaded")

  (local-set-key (kbd "C-c r") 'cargo-run)
  (local-set-key (kbd "C-c t") 'cargo-test)
  (local-set-key (kbd "C-c v") 'cargo-valgrind)
  (setq dash-at-point-docset "rust")
  (start-program-short-cut)
  ;; compatible with flyspell.
  (electric-spacing-mode))


;; Short cut Hooks here.
;; ==================================================================
(add-hook 'emacs-lisp-mode-hook 'elisp-short-cut)
(add-hook 'clojure-mode-hook    'clojure-short-cut)
(add-hook 'typescript-mode-hook 'typescript-short-cut)
(add-hook 'json-mode-hook       'json-short-cut)
(add-hook 'c-mode-common-hook   'c-common-short-cut)
(add-hook 'objc-mode-hook       'objc-short-cut)
(add-hook 'python-mode-hook     'python-short-cut)
(add-hook 'awk-mode-hook        'awk-short-cut);; After emacs 21 work here.
(add-hook 'sh-mode-hook         'shell-short-cut)
(add-hook 'LaTex-mode-hook      'tex-short-cut)
(add-hook 'js2-mode-hook        'js2-short-cut)
(add-hook 'jade-mode-hook       'jade-short-cut)
(add-hook 'rust-mode-hook       'rust-short-cut)
(when (string= system-type "windows-nt")
  (autoload 'powershell "powershell" "DOCSTRING" t)
  )


(provide 'dev-settings)
;; dev-settings ends here.
;;;
