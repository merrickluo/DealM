;; -*- Emacs-Lisp -*-
;; Last modified: <2016-09-20 18:04:53 Tuesday by richard>

;; Copyright (C) 2016 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1
;; PUBLIC LICENSE: GPLv3

(use-package org-tree-slide
  :after (org)
  :commands (org-tree-slide-mode))

(use-package org
  :commands (org-mode)
  :init
  (setq org-default-notes-file (concat emacs-root-path "notes.org")
        org-capture-templates
        '(("t" "Todo" entry
           (file+headline (concat emacs-root-path ".todo.org") "Tasks")
           "* TODO %?\n %i\n %a")
          ("j" "Journal" entry
           (file+datetree (concat emacs-root-path ".journey.org"))
           "* %?\nEntered on %U\n %i\n %a")))
  :config
  (defun org-settings()
    "org-mode-settings"
    (setq org-startup-folded                             nil
          org-cycle-include-plain-lists                  t
          org-src-fontify-natively                       t
          org-export-kill-product-buffer-when-displayed  t
          word-wrap                                      nil
          org-hide-leading-stars                         t
          org-src-window-setup                           'current-window)
    (turn-off-auto-fill)
    (org-indent-mode)
    (visual-line-mode)

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((ruby . t)
       (sh . t)
       (python . t)
       (emacs-lisp . t)))

    (unless (boundp 'org-latex-classes)
      (setq org-latex-classes nil))
    (add-to-list 'org-latex-classes
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

    (add-to-list 'org-latex-classes

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

    (add-to-list 'org-latex-classes
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
    (defun calendar-window-live-p ()
      "calendar window live "
      (window-live-p (get-buffer-window "*Calendar*" t)))
    (defmacro calendar-defkey (KEY CALENDAR_DEF ORIGIN_DEF)
      "provide key and if calendar window finded, do a calendar move;
origin move otherwise "
      `(define-key minibuffer-local-map ,KEY
         (lambda () (interactive)
           (if (calendar-window-live-p)
               (org-eval-in-calendar ,CALENDAR_DEF)
             (call-interactively ,ORIGIN_DEF))
           )
         ))

    (calendar-defkey (kbd "M-b") '(calendar-backward-month 1) 'backward-word)
    (calendar-defkey (kbd "M-f") '(calendar-forward-month 1) 'forward-word)
    (calendar-defkey (kbd "C-e") '(calendar-end-of-week 1) 'move-end-of-line)
    (calendar-defkey (kbd "C-a") '(calendar-beginning-of-week 1) 'smart-move-beginning-of-line)
    (calendar-defkey (kbd "C-M-b") '(calendar-backward-year 1) 'backward-sexp)
    (calendar-defkey (kbd "C-M-f") '(calendar-forward-year 1) 'forward-sexp)
    (calendar-defkey (kbd "C-p") '(calendar-backward-week 1) 'previous-line)
    (calendar-defkey (kbd "C-n") '(calendar-forward-week 1) 'next-line)
    (calendar-defkey (kbd "C-b") '(calendar-backward-day 1) 'backward-char)
    (calendar-defkey (kbd "C-f") '(calendar-forward-day 1) 'forward-char))
  (add-hook 'org-mode-hook 'org-settings))

(use-package org-agenda
  :bind (:map org-agenda-mode-map
              ("'" . switch-to-other-buffer)
              ("1" . delete-other-windows)
              ("2" . split-window-vertically)
              ("3" . split-window-horizontally)
              ("o" . other-window)))


(use-package org-colview
  :defer t  ; :commands, :bind*?, :bind-keymap*?, :mode, :interpreter implies
  :config   ; execute code after a package is loaded
  (org-defkey org-columns-map "e" 'org-columns-edit-value-sb)
  (org-defkey org-columns-map "h" 'backward-char)
  (org-defkey org-columns-map "l" 'forward-char)
  (org-defkey org-columns-map "j" 'next-line)
  (org-defkey org-columns-map "k" 'previous-line)
  (define-key org-columns-map "f" (key-binding (kbd "M-f")))
  (define-key org-columns-map "b" (key-binding (kbd "M-b"))))


(provide 'org-settings)
;; org-settings ends here.
;;;
