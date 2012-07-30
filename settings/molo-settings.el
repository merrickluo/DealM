;; -*- Emacs-Lisp -*-
;; Last modified: <2012-07-30 17:07:36 Monday by richard>

;; Copyright (C) 2012 Richard Wong

;; Author: Richard Wong
;; Email: chao787@gmail.com

;; Version: 0.1
;; PUBLIC LICENSE: GPLv3

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
(defun org-settings()
  "org-mode-settings"
  (setq org-startup-folded nil
        org-cycle-include-plain-lists t
        org-export-kill-product-buffer-when-displayed t)
  (setq org-export-latex-classes nil)
  (add-to-list 'org-export-latex-classes
               '("latex-org-report-article"
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
(add-to-list 'load-path (concat plugins-path-r "auctex/"))
(load "auctex" nil)
(load "preview-latex" nil)

(setq TeX-auto-save  t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)

;; (load "tex-buf" nil)

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
