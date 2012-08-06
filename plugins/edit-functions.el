;; -*- Emacs-Lisp -*-
;; Last modified: <2012-08-06 19:54:00 Monday by richard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 1.4
;; PUBLIC LICENSE: GPLv3

(require 'ahei-misc)

;;;###autoload
(defun gbk-to-utf-8()
  "Set current buffer from gbk to UTF-8"
  (interactive)
  (revert-buffer-with-coding-system 'gbk)
  (set-buffer-file-coding-system 'utf-8))

;;;###autoload
(defun unset-gbk-to-utf-8()
  "unset current buffer from gbk to UTF-8"
  (interactive)
  (revert-buffer-with-coding-system 'utf-8))

;;;###autoload
(defun font-big ()
  (interactive)
  (set-face-attribute 'default nil :height
                      (min 200
                           (+ (face-attribute 'default :height) 10))))

;;;###autoload
(defun font-small ()
  (interactive)
  (set-face-attribute 'default nil :height
                      (max 100
                           (- (face-attribute 'default :height) 10))))

;;;###autoload
(defun dos2unix ()
  "Change the current buffer coding system to unix."
  (interactive)
  (set-buffer-file-coding-system 'unix 't))


;;;###autoload
(defun unix2dos ()
  "Change the current buffer coding system to dos."
  (interactive)
  (set-buffer-file-coding-system 'dos 't))

;;;###autoload
(defun sudo()
  "Using the tramp to pseudo a sudo command in system(linux only)
to access the high privilige files...."
  (interactive)
  (let ((temp-buffer-name (buffer-file-name)))
    (if temp-buffer-name
        (progn
          (kill-buffer (current-buffer))
          (find-file (concat "/sudo::" temp-buffer-name ))
          (message "sudo succeed! Warn: The privilege is root!"))
      (message "This buffer %s's corresponding file cannot be sudo(ed)." (buffer-name)))))

;;;###autoload
(defun backward-kill-word-or-kill-region ()
  "`mark-active'时, 剪切选择的区域, 平时向后删除word, 和bash下面一样."
  (interactive)
  (if (rm-mark-active)
      (call-interactively 'rm-kill-region)
    (if mark-active
        (if (and (boundp 'use-cua) cua--rectangle)
            (progn
              (cua-cut-rectangle t)
              (cua-cancel))
          (call-interactively 'kill-region))
      (call-interactively 'backward-kill-word))))


;;;###autoload
(defun mark-whole-sexp (&optional not-whole)
  "Mark whole sexp.
If NOT-WHOLE is non-nil, do not mark whole sexp."
  (interactive "P")
  (if not-whole
      (mark-sexp)
    (let ((region (bounds-of-thing-at-point 'sexp)))
      (if (not region)
          (message "Can not found sexp.")
        (goto-char (car region))
        (call-interactively 'set-mark-command)
        (forward-sexp)))))

;;;###autoload
(defun kill-whole-sexp (&optional not-whole)
  "Kill whole sexp.
If NOT-WHOLE is non-nil, do not kill whole sexp."
  (interactive)
  (mark-whole-sexp not-whole)
  (when mark-active
    (kill-region (region-beginning) (region-end))
    (message "S-exp killed!")))

;;;###autoload
(defun copy-sexp (&optional not-whole)
  "Copy whole sexp.
If NOT-WHOLE is non-nil, do not copy whole sexp."
  (interactive)
  (save-excursion
    (mark-whole-sexp not-whole)
    (when mark-active
      (kill-ring-save (region-beginning) (region-end))
      (message "S-exp copied!"))))

;;;###autoload
(defun mark-word-1 ()
  ;;FIXME:
  "Mark this word. No whether the cursor inside the word or on the edge of the word."
  (interactive)
  (push-mark)
  (set-mark ))

;;;###autoload
(defun kill-word-1 ()
  "Delete this word. No whether the cursor inside the word or on the edge of the word."
  (interactive)
  (mark-word-1)
  (backward-kill-word-or-kill-region))

;;;###autoload
(defun mark-function ()
  "Mark function."
  (interactive)
  (cond
   ;; ((or (equal major-mode 'python-mode) (equal major-mode 'comint-mode))
   ;; (python-mark-funtion)
   ((or (equal major-mode 'c-mode) (equal major-mode 'c++-mode))
    (c-mark-function))
   ((or (equal major-mode 'emacs-lisp-mode) (equal major-mode 'lisp-mode) (equal major-mode 'lisp-interaction-mode))
    (lisp-mark-function))))

;;;###autoload
(defmacro def-action-on-function-command (fun-name action action-str)
  `(defun ,fun-name ()
     ,(concat (capitalize action-str) " function.")
     (interactive)
     (save-excursion
       (mark-function)
       (call-interactively ,action))))

;;;###autoload
(defun comment-function (&optional arg)
  "Comment function."
  (interactive "P")
  (save-excursion
    (mark-function)
    (comment-region (region-beginning) (region-end) arg)))

;;;###autoload
(defun kill-whole-paragraph (&optional arg)
  "Kill whole paragraph."
  (interactive "P")
  (if arg
      (kill-paragraph nil)
    (call-interactively 'mark-paragraph)
    (call-interactively 'kill-region)))

;;;###autoload
(defun copy-whole-paragraph (&optional arg)
  "Copy whole paragraph."
  (interactive "P")
  (save-excursion
    (if arg
        (progn
          (mark-command t)
          (forward-paragraph))
      (call-interactively 'mark-paragraph))
    (call-interactively 'kill-ring-save)))

;;;###autoload
(defun copy-cur-line ()
  "Copy current line"
  (interactive)
  (let ((end (min (point-max) (1+ (line-end-position)))))
    (copy-region-as-kill (line-beginning-position) end)))
(defalias 'copy-whole-line 'copy-cur-line)
;; compatible with kill-whole-line.

;;;###autoload
(defun copy-lines (&optional number)
  "从当前行开始拷贝NUMBER行"
  (interactive "p")
  (if (null number)
      (copy-cur-line)
    (let ((lineNo))
      (save-excursion
        (if (< number 0)
            (next-line))
        (setq lineNo (line-number-at-pos nil))
        (move-beginning-of-line nil)
        (set-mark-command nil)
        (goto-line (+ number lineNo))
        (call-interactively 'copy-region-as-kill-nomark)))))

;;;###autoload
(defun copy-line-left ()
  "拷贝当前行光标后面的文字"
  (interactive)
  (copy-region-as-kill-nomark (point) (min (1+ (line-end-position)) (point-max))))


;;;###autoload
(defun which-copy ()
  "如果`mark-active'的话, 则`copy-region-and-paste', 否则`copy-line-left'"
  (interactive)
  (if mark-active (copy-region-and-paste) (copy-line-left)))

;;;###autoload
(defun insert-cur-sexp ()
  "拷贝当前sexp并粘贴进当前buffer"
  (interactive)
  (copy-sexp)
  (call-interactively 'yank))

;;;###autoload
(defun copy-sentence ()
  "拷贝sentence"
  (interactive)
  (save-excursion
    (call-interactively 'mark-end-of-sentence)
    (call-interactively 'copy-region-as-kill-nomark)))

;; 删除当前光标到行首的字符
;;;###autoload
(defun del-to-begin (&optional arg)
  "Delete characters to line beginning."
  (interactive "P")
  (if (not arg)
      (kill-line 0)
    (copy-region-as-kill-nomark (1+ (line-beginning-position)) (point))))

;;;###autoload
(defun lisp-mark-function (&optional allow-extend)
  "`mark-defun'有时候会多mark一个空白行, 这个函数就是解决这个bug的"
  (interactive "p")
  (mark-defun allow-extend)
  (let (next-is-fun)
    (save-excursion (forward-line) (setq next-is-fun (looking-at "[ \t]*(defun")))
    (if (or (looking-at "$") (and next-is-fun (not (looking-at "[ \t]*(defun"))))
        (forward-line))))

;;;###autoload
(defun lisp-mark-function-name (&optional allow-extend)
  "`mark-defun'有时候会多mark一个空白行, 这个函数就是解决这个bug的"
  (interactive "p")
  (mark-defun allow-extend)
  (let (next-is-fun)
    (save-excursion (forward-line) (setq next-is-fun (looking-at "[ \t]*(defun")))
    (if (or (looking-at "$") (and next-is-fun (not (looking-at "[ \t]*(defun"))))
        (forward-line))))

;;;###autoload
(defun case-trans ()
  "大小写转换当前字符"
  (interactive)
  (let* ((ochar (char-after (point))) (char ochar))
    (if (and (>= char ?a) (<= char ?z))
        (setq char (upcase char))
      (setq char (downcase char)))
    (if (/= ochar char)
        (save-excursion
          (delete-char 1)
          (insert-char char 1)))))

;;;###autoload
(defun comment (&optional arg)
  "如果`mark-active'的话,就`comment-region',否则注释光标所在行"
  (interactive "P")
  (if mark-active
      (comment-region (region-beginning) (region-end) arg)
    (let (fun)
      (if arg (setq fun 'uncomment-region) (setq fun 'comment-region))
      (funcall fun (line-beginning-position) (line-end-position)))))

;;;###autoload
(defun uncomment (&optional arg)
  "如果`mark-active'的话,就`uncomment-region',否则取消注释光标所在行"
  (interactive "P")
  (comment (not arg)))

;;;###autoload
(defun mark-invisible-region ()
  "Mark invisible region."
  (interactive)
  (if (not (and last-region-beg last-region-end))
      (message "No previous region.")
    (goto-char last-region-beg)
    (if last-region-is-rect
        (if last-region-use-cua
            (call-interactively 'cua-set-rectangle-mark)
          (call-interactively 'rm-set-mark))
      (call-interactively 'set-mark-command))
    (goto-char last-region-end)
    (if (and last-region-is-rect last-region-use-cua)
        (cua--activate-rectangle))))

;;;###autoload
(defun c-electric-backspace-kill ()
  "If `mark-active', run `kill-region', otherwise run `c-electric-backspace'."
  (interactive)
  (if mark-active
      (call-interactively 'kill-region)
    ;; (call-interactively 'c-electric-backspace)
    (call-interactively 'delete-backward-char)))

;;;###autoload
(defun delete-blank-lines-region (beg end)
  "Execute `delete-blank-lines' in region."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (let ((blank-line "^\\s-*$")
          (nonblank-line "^.*\\S-.*$")
          blank-beg blank-end)
      (while (and (< (point) end) (setq blank-beg (search-forward-regexp blank-line end t)))
        (save-excursion
          (setq blank-end (search-forward-regexp nonblank-line end t)))
        (if blank-end
            (setq end (- end (- blank-end blank-beg)))
          (setq end 0))
        (previous-line)
        (delete-blank-lines)))))

;;;###autoload
(defun smart-home (&optional home)
  "Goto home.

If HOME is negative, call `beginning-of-line-text',
otherwise call `move-beginning-of-line'."
  (interactive "P")
  (if (not home)
      (let ((old (point)))
        (beginning-of-line-text)
        (if (= (point) old)
            (move-beginning-of-line 1)))
    (if (< (prefix-numeric-value home) 0)
        (beginning-of-line-text)
      (move-beginning-of-line 1))))

(defun smart-move-beginning-of-line()
  "If the cursor is not at the current line's spaces, executes as call to `back-to-indentation' , otherwise as call `move-beginning-of-line'"
  (interactive "^")
  (let ((current-point (point)))
    (beginning-of-line 1)
    (skip-syntax-forward " " (line-end-position))
    (backward-prefix-chars)
    (when (>= (point) current-point)
      (beginning-of-line))))


(defun smart-next-line(&optional arg)
  "Documentation..."
  (interactive "P")
  (when (thing-at-point 'string)
    (call-interactively 'forward-line)
    (call-interactively '-)))

(defun smart-copy-line ()
  "If `mark-active', call `kill-ring-save', otherwise copy whole line."
  (interactive)
  (save-excursion
    (save-restriction
      (if mark-active
          (progn
            (call-interactively 'kill-ring-save)
            (message "selected region copied."))
        (if (thing-at-point 'string)
            (let ((beg (beginning-of-thing 'string)) (end (end-of-thing 'string)))
              (kill-ring-save beg end)
              (message "%s copied" (buffer-substring beg end)))
          (progn
            (call-interactively 'copy-whole-line)
            (message "current line copied")))))))
(defalias 'smart-copy 'smart-copy-line)

(defun program-smart-kill ()
  "If `mark-active', call `kill-region';
If At the first or end of line call `kill-whole-line';
If At the comment of the line, call `kill-comment'
If At the string (which inside \") of the line and string is not empty, kill the string inside.."
  (interactive)
  (if mark-active
      (call-interactively 'kill-region)
    (if (or (bolp) (eolp))
        (call-interactively 'kill-whole-line)
      (if (thing-at-point 'string)
          (kill-region (beginning-of-thing 'string)
                       (end-of-thing 'string))
        (call-interactively 'kill-line)))))

(defun smart-kill ()
  "If `mark-active', call `kill-region';
If At the first or end of line call `kill-whole-line';
If At the comment of the line, call `kill-comment'
If At the string (which inside \") of the line and string is not empty, kill the string inside.."
  (interactive)
  (if mark-active
      (call-interactively 'kill-region)
    (if (or (bolp) (eolp))
        (call-interactively 'kill-whole-line)
      (call-interactively 'kill-line))))


;;;###autoload
(defun smart-indent ()
  "If `mark-active', call `indent-region', otherwise indent all buffer."
  (interactive)
  (save-excursion
    (unless mark-active
      (call-interactively 'mark-whole-buffer))
    (call-interactively 'indent-region)))


;;;###autoload
(defun fill-paragraph-justify (region)
  "Run `fill-paragraph' with argument justify t."
  (interactive (list t))
  (fill-paragraph 'full region))


;; normal extending function
;; ==================================================================

(defun copy-file-name (&optional full)
  "Copy file name of current-buffer.
If FULL is t, copy full file name."
  (interactive "P")
  (let ((file (buffer-name)))
    (if full
        (setq file (expand-file-name file)))
    (kill-new file)
    (message "File name `%s' copied." file)))

(defun copy-file-name-no-nopostfix (&optional full)
  "If FULL is t, copy file name without postfix."
  (interactive "P")
  (let ((file (buffer-name)))
    (if full
        (setq file (expand-file-name file)))
    (kill-new file)))


;;;###autoload
(defun smart-insert-line ()
  "If `mark-active', then call `kill-region' one time and paste two time, otherwise insert this line."
  (interactive)
  ;; HACK:
  (if mark-active
      (progn (call-interactively 'kill-region)
             (call-interactively 'yank)
             (call-interactively 'yank))
    (progn (kill-ring-save (line-beginning-position)
                           (min (point-max )(1+ (line-end-position))))
           (forward-line)
           (beginning-of-line)
           (call-interactively 'yank)
           (previous-line)
           (end-of-line))))


;;;###autoload
(defun open-line-if-active-delete-then-open-line ()
  "If `mark-active', then and a call `kill-region', otherwise directly call `open-line'."
  (interactive)
  (when mark-active
    (call-interactively 'kill-region))
  (call-interactively 'open-line))

(defvar switch-major-mode-last-mode nil)

;;;###autoload
(defun switch-major-mode (mode)
  "切换major mode"
  (interactive
   (let ((fn switch-major-mode-last-mode) val)
     (setq val
           (completing-read
            (if fn (format "切换major-mode为(缺省为%s): " fn) "切换major mode为: ")
            obarray 'major-mode-heuristic t nil nil (symbol-name fn)))
     (list (intern val))))
  (let ((last-mode major-mode))
    (funcall mode)
    (setq switch-major-mode-last-mode last-mode)))

;;;###autoload
(defun get-mode-name ()
  "显示`major-mode'及`mode-name'"
  (interactive)
  (message "major-mode为%s, mode-name为%s" major-mode mode-name))

;;;###autoload
(defmacro def-action-on-area-command (fun-name action mark-area doc)
  `(defun ,fun-name ()
     ,doc
     (interactive)
     (save-excursion
       (funcall ,mark-area)
       (call-interactively ,action))))

;;;###autoload
(defun update-current-file-autoloads (file &optional save-after)
  "`update-file-autoloads' for current file."
  (interactive "fUpdate autoloads for file: \np")
  (let* ((load-file (expand-file-name "loaddefs.el"))
         (generated-autoload-file load-file))
    (unless (file-exists-p load-file)
      (shell-command (concat "touch " load-file)))
    (update-file-autoloads file save-after)))

(defun smart-line-kill ()
  "If `mark-active', call `kill-region', otherwise call `kill-whole-line'."
  (interactive)
  (if mark-active
      (call-interactively 'kill-region)
    (kill-region (line-beginning-position) (1- (line-end-position)))))

;;;###autoload
(defun copy-number-lines()
  "Copy the current line number"
  (interactive)
  (let ((line-number  (line-number-at-pos)))
    (kill-new (int-to-string line-number))
    (message "current line number: %d copied." line-number )))

(autoload 'org-open-file "org")

;;;###autoload
(defun open-file-with-app (file)
  "Open file with appropriate application."
  (interactive "fFile to open: ")
  (org-open-file file))

;;;###autoload
(defun open-current-file-with-app ()
  "Open current file with appropriate application."
  (interactive)
  (open-file-with-app buffer-file-name))

;;;###autoload
(defun major-mode-heuristic (symbol)
  (and (fboundp symbol)
       (string-match ".*-mode$" (symbol-name symbol))))


;;;###autoload
(defun smart-delete-blank-lines (&optional no-region)
  "Smart `delete-blank-lines'.

If NO-REGION is non-nil, always execute `delete-blank-lines',
otherwise, if `mark-active', execute `delete-blank-lines-region',
and execute `delete-blank-lines' if there no mark."
  (interactive "P")
  (if (or no-region (not mark-active))
      (delete-blank-lines)
    (call-interactively 'delete-blank-lines-region)))

;;;###autoload
(defun switch-to-scratch ()
  "Switch buffer to *scratch*"
  (interactive)
  (let ((buffer (get-buffer-create "*scratch*")))
    (switch-to-buffer buffer)
    (unless (equal major-mode 'lisp-interaction-mode)
      (lisp-interaction-mode))))

;;;###autoload
(defun switch-to-message ()
  "Switch buffer to *Messages*"
  (interactive)
  (let ((buffer (get-buffer-create "*Messages*")))
    (switch-to-buffer buffer)))

;;;###autoload
(defun switch-to-shell ()
  "Switch buffer to *shell*
If *shell* is not started, create one.
If *shell* is open and in front, move point to that window,
otherwise, change current buffer to that window.
"
  (interactive)
  (let ((flag nil)
        (count 1)
        (current-window (selected-window))
        (win-list (window-list))
        win dwin buf-name)

    (while win-list
      (setq win (pop win-list))
      (setq buf-name (buffer-name (window-buffer win)))
      (unless flag
        (setq flag (string-equal "*shell*" buf-name))
        (if flag
            (setq dwin win))))
    (if flag
        (select-window dwin)
      (shell))))

;;;###autoload
(defmacro def-redo-command (fun-name redo undo)
  "Make redo command."
  `(defun ,fun-name ()
     (interactive)
     (if (equal last-command ,redo)
         (setq last-command 'undo)
       (setq last-command nil))
     (call-interactively ,undo)
     (setq this-command ,redo)))

(def-redo-command redo 'redo 'undo)

;; uncategorized function.
;; ------------------------------------------------------------------
(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point))

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "copy thing between beg & end into kill ring"
  (let ((beg (get-point begin-of-thing 1))
        (end (get-point end-of-thing arg)))
    (copy-region-as-kill beg end)))

(defun paste-to-mark(&optional arg)
  "Paste things to mark, or to the prompt in shell-mode"
  (let ((pasteMe
         (lambda()
           (if (string= "shell-mode" major-mode)
               (progn (comint-next-prompt 25535) (yank))
             (progn (goto-char (mark)) (yank) )))))
    (if arg
        (if (= arg 1)
            nil
          (funcall pasteMe))
      (funcall pasteMe))))

;;;###autoload
(defmacro am-def-active-fun (symbol &optional fun-name)
  "Make definition of function judge variable is active or not."
  `(defun ,(if fun-name fun-name symbol) ()
     ,(concat "`" (symbol-name symbol) "' is t or not.")
     (am-variable-is-t ',symbol)))

(am-def-active-fun rm-mark-active rm-mark-active)

;;;###autoload
(defmacro def-position-command (fun-name key position)
  "Add postion command to specific key and position."
  `(progn
     (defun ,fun-name ()
       (interactive)
       (dired ,position))
     (global-set-key ,key ',fun-name)))

;;;###autoload
(defun goto-paren ()
  "跳到匹配的括号"
  (interactive)
  (cond
   ((looking-at "[ \t]*[[\"({]") (forward-sexp) (backward-char))
   ((or (looking-at "[]\")}]") (looking-back "[]\")}][ \t]*")) (if (< (point) (point-max)) (forward-char)) (backward-sexp))
   (t (message "找不到匹配的括号"))))

;;;###autoload
(defun ywb-indent-accoding-to-paren ()
  "按块([]{}())来格式化代码"
  (interactive)
  (let ((prev-char (char-to-string (preceding-char)))
        (next-char (char-to-string (following-char)))
        (pos (point)))
    (save-excursion
      (cond ((string-match "[[{(<]" next-char)
             (indent-region pos (progn (forward-sexp 1) (point)) nil))
            ((string-match "[\]})>]" prev-char)
             (indent-region (progn (backward-sexp 1) (point)) pos nil))))))


;; autoloads from Drew Adams
;; ------------------------------------------------------------------
(autoload 'end-of-line+                "misc-cmds" "" t)
(autoload 'region-length               "misc-cmds" "" t)
(autoload 'goto-longest-line           "misc-cmds" "" t)
(autoload 'goto-long-line              "misc-cmds" "" t)
(autoload 'region-to-buffer            "misc-cmds" "" t)
(autoload 'region-to-file              "misc-cmds" "" t)
(autoload 'resolve-file-name           "misc-cmds" "" t)
(autoload 'kill-buffer-and-its-windows "misc-cmds" "" t)
(autoload 'indirect-buffer             "misc-cmds" "" t)
(autoload 'clear-search-ring           "misc-cmds" "" t)
(autoload 'clear-regexp-search-ring    "misc-cmds" "" t)

(provide 'edit-functions)
;; edit-functions ends here.
;;;
