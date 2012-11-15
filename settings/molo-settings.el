;; -*- Emacs-Lisp -*-
;; Last modified: <2012-11-15 14:39:15 Thursday by richard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.2
;; PUBLIC LICENSE: GPLv3

;; molo is short for Markdown Org latex and orz..
;; file contains about all org-related settings.
;; 1. markdown-mode settings.
;; 2. org-mode settings.
;; 3. latex-mode settings.
;; 4. some other note settings.

;; Markdown-mode settings
;; ------------------------------------------------------------------
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.te?xt" . markdown-mode) auto-mode-alist))


;; org-mode settings
;; ------------------------------------------------------------------
(setq org-default-notes-file (concat emacs-root-path "notes.org"))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (concat emacs-root-path ".todo.org") "Tasks")
         "* TODO %?\n %i\n %a")
        ("j" "Journal" entry (file+datetree (concat emacs-root-path ".journey.org"))
         "* %?\nEntered on %U\n %i\n %a")))

(defun org-settings()
  "org-mode-settings"
  (setq org-startup-folded nil
        org-cycle-include-plain-lists t
        org-export-kill-product-buffer-when-displayed t)
  ;;FIXME: org-babel-execution.
  ;; for c++, python, ruby, and elisp
  ;; #+LaTeX_CLASS: beamer in org files
  (autoload 'org-set-generic-type "org-export-generic")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '( (perl . t)
      (ruby . t)
      (sh . t)
      (python . t)
      (emacs-lisp . t)
      ))

  (org-set-generic-type
   "Markdown"
   '(:file-suffix ".markdown"
                  :key-binding ?M
                  :title-format "%s\n"
                  :title-suffix ?=
                  :body-header-section-numbers t
                  :body-header-section-number-format "%s) "
                  :body-section-header-prefix	("\n## " "\n### " "\n#### " "\n##### " "\n###### ")
                  :body-section-header-format	"%s"
                  :body-section-header-suffix "\n"
                  :todo-keywords-export t
                  :body-line-format "  %s\n"
                  :body-tags-export	t
                  :body-tags-prefix	" <tags>"
                  :body-tags-suffix	"</tags>\n"
                  :body-section-prefix	"<secprefix>\n"
                  :body-section-suffix	"</secsuffix>\n"
                  :body-line-export-preformated	t
                  :body-line-fixed-prefix	"<pre>\n"
                  :body-line-fixed-suffix	"\n</pre>\n"
                  :body-line-fixed-format	"%s\n"
                  :body-list-prefix	"\n"
                  :body-list-suffix	"\n"
                  :body-list-format	"  * %s\n"
                  :body-number-list-prefix	"<ol>\n"
                  :body-number-list-suffix	"</ol>\n"
                  :body-number-list-format	"<li>%s</li>\n"
                  ;; :body-number-list-leave-number	t
                  :body-list-checkbox-todo	"[_] "
                  :body-list-checkbox-todo-end	""
                  :body-list-checkbox-done	"[X] "
                  :body-list-checkbox-done-end ""
                  :body-line-format	"%s"
                  :body-line-wrap	75
                  :body-text-prefix	""
                  :body-text-suffix	""
                  ))

  (unless (boundp 'org-export-latex-classes)
    (setq org-export-latex-classes nil))
  (add-to-list 'org-export-latex-classes
               ;; beamer class, for presentations
               '("beamer"
                 "\\documentclass[11pt]{beamer}\n
      % UTF-8 encoding\n
      \\mode<{{{beamermode}}}>\n
      \\usetheme{{{{beamertheme}}}}\n
      \\usecolortheme{{{{beamercolortheme}}}}\n
      \\beamertemplateballitem\n
      \\setbeameroption{show notes}
      \\usepackage{xeCJK}
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{hyperref}\n
      \\usepackage{color}
      \\usepackage{listings}
      \\setCJKmainfont{Microsoft YaHei}
      \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
  frame=single,
  basicstyle=\\small,
  showspaces=false,showstringspaces=false,
  showtabs=false,
  keywordstyle=\\color{blue}\\bfseries,
  commentstyle=\\color{red},
  }\n
      \\usepackage{verbatim}\n
      \\institute{{{{beamerinstitute}}}}\n
       \\subject{{{{beamersubject}}}}\n"

                 ("\\section{%s}" . "\\section*{%s}")

                 ("\\begin{frame}[fragile]\\frametitle{%s}"
                  "\\end{frame}"
                  "\\begin{frame}[fragile]\\frametitle{%s}"
                  "\\end{frame}")))


  ;; letter class, for formal letters

  (add-to-list 'org-export-latex-classes

               '("letter"
                 "\\documentclass[11pt]{letter}\n
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{color}"

                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-export-latex-classes
               '("chinese-export"
                 "% Compile with xelatex
% UTF-8 encoding
\\documentclass{article}
\\usepackage{xeCJK}
\\setCJKmainfont{Microsoft YaHei}
\\pagestyle{empty}
\\title{}
      [NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (setq org-latex-to-pdf-process
        '("xelatex -interaction nonstopmode %f"
          "xelatex -interaction nonstopmode %f")) ;; for multiple passes

  (defun org-kill-whole-line  (&optional arg)
    "Kill line, to tags or end of line.
If `mark-active', call `kill-region';
If At the first or end of line call `kill-whole-line';
If At the string (which inside \") of the line and string is not empty, kill the string inside.."
    (interactive "P")
    (cond
     (mark-active
      (call-interactively 'kill-region))
     ((or (bolp)
          ;; (not org-special-ctrl-k)
          ;; (not (org-on-heading-p))
          (eolp))
      (call-interactively 'kill-whole-line))
     ((looking-at (org-re ".*?\\S-\\([ \t]+\\(:[[:alnum:]_@:]+:\\)\\)[ \t]*$"))
      (kill-region (point) (match-beginning 1))
      (org-set-tags nil t))
     (t (kill-region (point) (point-at-eol)))))

  (defun org-literal-links ()
    "Show literal links."
    (interactive)
    (org-remove-from-invisibility-spec '(org-link)) (org-restart-font-lock))

  (defun org-descriptive-links ()
    "Show descriptive links."
    (interactive)
    (org-add-to-invisibility-spec '(org-link)) (org-restart-font-lock))

  (defun org-display-content ()
    "Display content in `org-mode'."
    (interactive)
    (org-overview)
    (org-content))

  (defvar org-display-content nil "Display content or not now.")

  (defvar org-fold-subtree nil "Fold subtree or not now.")

  (defun org-toggle-display-content ()
    "Toggle display content."
    (interactive)
    (setq org-display-content (not org-display-content))
    (if org-display-content
        (org-display-content)
      (show-all)))

  (define-key org-mode-map (kbd "C-c e") 'org-table-edit-field)
  (define-key org-mode-map (kbd "C-k")   'org-kill-whole-line)
  (define-key org-mode-map (kbd "<tab>") nil)
  (define-key org-mode-map (kbd "C-j")   nil)
  (define-key org-mode-map (kbd "C-c e") 'org-table-edit-field)
  (define-key org-mode-map (kbd "C-c n") 'org-forward-same-level)
  (define-key org-mode-map (kbd "C-c p") 'org-backward-same-level)
  (define-key org-mode-map (kbd "C-M-f") 'org-do-demote)
  (define-key org-mode-map (kbd "C-M-b") 'org-do-promote)

  ;; minibuffer in org mode
  ;; ---------------------------------[minibuffer in org mode]
  (org-defkey minibuffer-local-map (kbd "M-b")
              (lambda () (interactive)
                (org-eval-in-calendar '(calendar-backward-month 1))))
  (org-defkey minibuffer-local-map (kbd "M-f")
              (lambda () (interactive)
                (org-eval-in-calendar '(calendar-forward-month 1))))
  (org-defkey minibuffer-local-map (kbd "C-M-b")
              (lambda () (interactive)
                (org-eval-in-calendar '(calendar-backward-year 1))))
  (org-defkey minibuffer-local-map (kbd "C-e")
              (lambda () (interactive)
                (org-eval-in-calendar '(calendar-end-of-week 1))))
  (org-defkey minibuffer-local-map (kbd "C-a")
              (lambda () (interactive)
                (org-eval-in-calendar '(calendar-beginning-of-week 1))))
  (org-defkey minibuffer-local-map (kbd "C-M-f")
              (lambda () (interactive)
                (org-eval-in-calendar '(calendar-forward-year 1))))
  (org-defkey minibuffer-local-map (kbd "C-p")
              (lambda () (interactive)
                (org-eval-in-calendar '(calendar-backward-week 1))))
  (org-defkey minibuffer-local-map (kbd "C-n")
              (lambda () (interactive)
                (org-eval-in-calendar '(calendar-forward-week 1))))
  (org-defkey minibuffer-local-map (kbd "C-b")
              (lambda () (interactive)
                (org-eval-in-calendar '(calendar-backward-day 1))))
  (org-defkey minibuffer-local-map (kbd "C-f")
              (lambda () (interactive)
                (org-eval-in-calendar '(calendar-forward-day 1))))


  )
(defun org-agenda-settings()
  "org-agenda settings"
  (define-key org-agenda-mode-map "'" 'switch-to-other-buffer)
  (define-key org-agenda-mode-map "1" 'delete-other-windows)
  (define-key org-agenda-mode-map "2" 'split-window-vertically)
  (define-key org-agenda-mode-map "3" 'split-window-horizontally)
  (define-key org-agenda-mode-map "o" 'other-window))

(defun org-colview-settings ()
  "Settings for `org-colview'."
  (org-defkey org-columns-map "e" 'org-columns-edit-value-sb)
  (org-defkey org-columns-map "h" 'backward-char)
  (org-defkey org-columns-map "l" 'forward-char)
  (org-defkey org-columns-map "j" 'next-line)
  (org-defkey org-columns-map "k" 'previous-line)
  (define-key org-columns-map "f" (key-binding (kbd "M-f")))
  (define-key org-columns-map "b" (key-binding (kbd "M-b"))))

(eval-after-load "org"
  '(org-settings))

(eval-after-load "org-agenda"
  '(org-agenda-settings))

(eval-after-load "org-colview"
  '(org-colview-settings))



;; latex-mode settings.
;; ------------------------------------------------------------------
(when (string= system-type "gnu/linux")

  (defun load-latex()
    "load latex settings for org-mode or for itself."
    (interactive)
    (add-to-list 'load-path (concat plugins-path-r "auctex/"))
    (load "auctex" nil)
    (load "preview-latex" nil)

    (setq TeX-auto-save  t)
    (setq TeX-parse-self t)
    (setq TeX-save-query nil)
    ;; (load "tex-buf" nil)
    )

  (defun run-latexmk ()
    (interactive)
    (let ((TeX-save-query nil)
          (TeX-process-asynchronous nil)
          (master-file (TeX-master-file)))
      (TeX-save-document "")
      (TeX-run-TeX "latexmk" "latexmk" master-file)
      (if (plist-get TeX-error-report-switches (intern master-file))
          (TeX-next-error t)
        (minibuffer-message "latexmk done"))))
  )


;; hooks
;; ------------------------------------------------------------------
(defun pdfevince ()
  (add-to-list 'TeX-output-view-style
               (quote ("^pdf$" "." "evince %o %(outpage)"))))


(mapc (lambda (mode)
        (add-hook 'LaTeX-mode-hook mode))
      (list 'auto-fill-mode
            'LaTeX-math-mode
            'turn-on-reftex
            'linum-mode))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (setq TeX-auto-untabify t     ; remove all tabs before saving
                  TeX-engine 'xetex       ; use xelatex default
                  TeX-show-compilation t) ; display compilation windows
            (TeX-global-PDF-mode t)       ; PDF mode enable, not plain
            (setq TeX-save-query nil)
            (imenu-add-menubar-index)
            (flyspell-mode t)
            (local-set-key (kbd "C-0") 'run-latexmk)
            (define-key LaTeX-mode-map (kbd "TAB") 'TeX-complete-symbol)
            (pdfevince)))

(provide 'molo-settings)
;; molo-settings ends here.
;;;
